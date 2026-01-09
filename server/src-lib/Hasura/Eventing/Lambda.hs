-- | AWS Lambda URL transformation and signing.
--
-- This module provides transparent Lambda support by transforming
-- @aws://@ URLs into signed HTTPS requests to the AWS Lambda API.
--
-- Lambda is just HTTP with:
-- 1. URL: @https://lambda.{region}.amazonaws.com/2015-03-31/functions/{name}/invocations@
-- 2. Headers: AWS Signature V4 + @X-Amz-Invocation-Type@
--
-- Usage:
-- @
-- aws://arn:aws:lambda:us-east-1:123456789:function:myFunc
-- aws://arn:aws:lambda:us-east-1:123456789:function:myFunc?invocation_type=event
-- @
module Hasura.Eventing.Lambda
  ( -- * Request creation
    mkLambdaAwareRequest,
    isLambdaUrl,

    -- * Request transformation
    transformLambdaRequest,

    -- * Response handling
    checkLambdaFunctionError,

    -- * AWS Credentials
    AWSCredentials (..),
    getAWSCredentials,
  )
where

import Control.Lens (preview, set, view)
import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hasura.Prelude
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.HTTP.Types qualified as HTTPTypes
import Data.Environment qualified as Env
import Debug.Trace (trace, traceIO)

--------------------------------------------------------------------------------
-- Constants

-- | Header used to preserve the original Lambda URL through request transformations.
-- The HTTP.url lens doesn't preserve non-HTTP schemes like @aws://@, so we store
-- the original URL in this header and read it back in 'transformLambdaRequest'.
lambdaUrlHeader :: CI.CI BS.ByteString
lambdaUrlHeader = "X-Hasura-Lambda-URL"

--------------------------------------------------------------------------------
-- AWS Credentials

-- | AWS credentials for signing requests.
data AWSCredentials = AWSCredentials
  { _acAccessKeyId :: Text,
    _acSecretAccessKey :: Text,
    _acSessionToken :: Maybe Text
    -- ^ Session token for temporary credentials (STS, IAM roles)
  }
  deriving (Show, Eq)

-- | Get AWS credentials from environment variables.
-- Returns Nothing if AWS_ACCESS_KEY_ID or AWS_SECRET_ACCESS_KEY are not set.
getAWSCredentials :: Env.Environment -> Maybe AWSCredentials
getAWSCredentials env =
  let mAccessKey = Env.lookupEnv env "AWS_ACCESS_KEY_ID"
      mSecretKey = Env.lookupEnv env "AWS_SECRET_ACCESS_KEY"
      mSessionToken = Env.lookupEnv env "AWS_SESSION_TOKEN"
   in trace ("[Lambda] Looking up credentials: AWS_ACCESS_KEY_ID=" <> show (isJust mAccessKey) <> " AWS_SECRET_ACCESS_KEY=" <> show (isJust mSecretKey)) $
        case (mAccessKey, mSecretKey) of
          (Just accessKey, Just secretKey) ->
            Just
              AWSCredentials
                { _acAccessKeyId = T.pack accessKey,
                  _acSecretAccessKey = T.pack secretKey,
                  _acSessionToken = T.pack <$> mSessionToken
                }
          _ -> Nothing

--------------------------------------------------------------------------------
-- Request creation

-- | Check if a URL is a Lambda URL (starts with @aws://@).
isLambdaUrl :: Text -> Bool
isLambdaUrl = T.isPrefixOf "aws://"

-- | Create an HTTP request, handling both regular HTTP(S) URLs and Lambda URLs.
--
-- For Lambda URLs (@aws://@), creates a placeholder request with a dummy HTTPS URL
-- that will be transformed later by 'transformLambdaRequest'. The original Lambda URL
-- is stored in the request URL field so that 'transformLambdaRequest' can read it.
--
-- For regular URLs, delegates to 'HTTP.mkRequestEither'.
mkLambdaAwareRequest :: Text -> Either HTTPClient.HttpException HTTP.Request
mkLambdaAwareRequest url = trace ("[Lambda] mkLambdaAwareRequest: " <> T.unpack url <> " isLambda=" <> show (isLambdaUrl url)) $
  if isLambdaUrl url
    then
      -- For Lambda URLs, create a placeholder request with a valid HTTPS URL structure
      -- that will be replaced by transformLambdaRequest. We store the original aws://
      -- URL in a custom header because the HTTP.url lens doesn't preserve non-HTTP schemes.
      case HTTP.mkRequestEither "https://lambda.placeholder.amazonaws.com/" of
        Left err -> Left err
        Right req ->
          Right $
            req & set HTTP.headers [(lambdaUrlHeader, TE.encodeUtf8 url)]
    else HTTP.mkRequestEither url

--------------------------------------------------------------------------------
-- Request transformation

-- | Transform an HTTP request for Lambda if needed.
-- If the URL starts with @aws://@ scheme, transforms it to a signed
-- HTTPS request to the AWS Lambda Invoke API.
-- Otherwise returns the request unchanged.
transformLambdaRequest :: Env.Environment -> HTTP.Request -> IO HTTP.Request
transformLambdaRequest env req = do
  -- Read the Lambda URL from our custom header (the HTTP.url lens doesn't preserve aws:// scheme)
  let reqUrl = getLambdaUrl req
  case parseLambdaUrl reqUrl of
    Nothing -> pure req -- Not a Lambda URL, pass through
    Just (region, functionName, invocationType) -> do
      let lambdaEndpoint = "https://lambda." <> region <> ".amazonaws.com/2015-03-31/functions/" <> functionName <> "/invocations"
      traceIO $ "[Lambda] ARN: " <> T.unpack reqUrl <> " -> " <> T.unpack lambdaEndpoint
      case getAWSCredentials env of
        Nothing -> do
          traceIO "[Lambda] ERROR: No AWS credentials (AWS_ACCESS_KEY_ID/AWS_SECRET_ACCESS_KEY not set)"
          pure req
        Just creds -> do
          traceIO $ "[Lambda] Signing request for " <> T.unpack functionName <> " in " <> T.unpack region
          buildLambdaRequest creds region functionName invocationType req

-- | Get the Lambda URL from a request, checking the custom header first.
getLambdaUrl :: HTTP.Request -> Text
getLambdaUrl req =
  let headers = view HTTP.headers req
   in case lookup lambdaUrlHeader headers of
        Just urlBytes -> TE.decodeUtf8 urlBytes
        Nothing -> view HTTP.url req

-- | Parse a Lambda URL to extract region, function name, and invocation type.
-- Supports format: @aws://arn:aws:lambda:{region}:{account}:function:{name}?invocation_type=event@
parseLambdaUrl :: Text -> Maybe (Text, Text, Text)
parseLambdaUrl url
  | Just arnPart <- T.stripPrefix "aws://" url =
      let (arn, queryPart) = T.breakOn "?" arnPart
          invocationType = parseInvocationType queryPart
       in case parseArn arn of
            Just (region, functionName) -> Just (region, functionName, invocationType)
            Nothing -> Nothing
  | otherwise = Nothing
  where
    parseArn :: Text -> Maybe (Text, Text)
    parseArn arn = case T.splitOn ":" arn of
      ["arn", "aws", "lambda", region, _account, "function", name] ->
        Just (region, name)
      ["arn", "aws", "lambda", region, _account, "function", name, qualifier] ->
        Just (region, name <> ":" <> qualifier)
      _ -> Nothing

    parseInvocationType :: Text -> Text
    parseInvocationType queryStr
      | T.null queryStr = "RequestResponse"
      | "invocation_type=event" `T.isInfixOf` T.toLower queryStr = "Event"
      | otherwise = "RequestResponse"

-- | Build a signed Lambda request.
buildLambdaRequest ::
  AWSCredentials ->
  Text -> -- region
  Text -> -- function name
  Text -> -- invocation type
  HTTP.Request ->
  IO HTTP.Request
buildLambdaRequest creds region functionName invocationType originalReq = do
  now <- getCurrentTime

  let host = "lambda." <> region <> ".amazonaws.com"
      path = "/2015-03-31/functions/" <> functionName <> "/invocations"
      lambdaUrl = "https://" <> host <> path

      -- Get the payload from the original request body
      payload = case preview HTTP._RequestBodyLBS (view HTTP.body originalReq) of
        Just lbs -> LBS.toStrict lbs
        Nothing -> "" -- For streaming bodies, use empty (shouldn't happen for webhooks)

      -- Base headers for Lambda
      baseHeaders =
        [ ("Host", TE.encodeUtf8 host),
          ("X-Amz-Invocation-Type", TE.encodeUtf8 invocationType),
          ("Content-Type", "application/json")
        ]
          ++ maybe [] (\t -> [("X-Amz-Security-Token", TE.encodeUtf8 t)]) (_acSessionToken creds)

  -- Sign the request
  signedHeaders <- signRequest creds region "lambda" "POST" path baseHeaders payload now

  -- Build the new request using lenses
  case HTTP.mkRequestEither lambdaUrl of
    Left _ -> pure originalReq -- Failed to parse, return original
    Right baseReq ->
      pure $
        baseReq
          & set HTTP.method "POST"
          & set HTTP.headers signedHeaders
          & set HTTP.body (HTTPClient.RequestBodyBS payload)
          & set HTTP.timeout (view HTTP.timeout originalReq)

--------------------------------------------------------------------------------
-- Response handling

-- | Check if a Lambda response indicates a function error.
-- Lambda returns HTTP 200 even when the function fails - the error is
-- indicated by the @X-Amz-Function-Error@ header.
-- Returns the error type if present, Nothing otherwise.
checkLambdaFunctionError :: HTTPClient.Response body -> Maybe Text
checkLambdaFunctionError resp =
  let headers = HTTPClient.responseHeaders resp
   in TE.decodeUtf8 <$> lookup "X-Amz-Function-Error" headers

--------------------------------------------------------------------------------
-- AWS Signature V4

-- | Sign an AWS request using Signature Version 4.
-- https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_sigv.html
signRequest ::
  AWSCredentials ->
  Text -> -- region
  Text -> -- service (e.g., "lambda")
  BS.ByteString -> -- method
  Text -> -- path
  [(BS.ByteString, BS.ByteString)] -> -- headers
  BS.ByteString -> -- payload
  UTCTime ->
  IO [HTTPTypes.Header]
signRequest creds region service method path headers payload now = do
  let accessKey = TE.encodeUtf8 $ _acAccessKeyId creds
      secretKey = TE.encodeUtf8 $ _acSecretAccessKey creds

      -- Date strings
      amzDate = BS8.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now
      dateStamp = BS8.pack $ formatTime defaultTimeLocale "%Y%m%d" now

      -- Add required headers
      allHeaders =
        sort $
          ("x-amz-date", amzDate) : headers

      -- Canonical request components
      canonicalUri = TE.encodeUtf8 path
      canonicalQuerystring = "" -- No query params for Lambda Invoke
      signedHeadersList = sort $ map (CI.foldedCase . CI.mk . fst) allHeaders
      signedHeaders = BS.intercalate ";" signedHeadersList
      canonicalHeaders =
        BS.concat
          [ CI.foldedCase (CI.mk k) <> ":" <> v <> "\n"
            | (k, v) <- sortBy (comparing (CI.foldedCase . CI.mk . fst)) allHeaders
          ]
      payloadHash = hashHex payload

      canonicalRequest =
        BS.intercalate
          "\n"
          [ method,
            canonicalUri,
            canonicalQuerystring,
            canonicalHeaders,
            signedHeaders,
            payloadHash
          ]

      -- String to sign
      algorithm = "AWS4-HMAC-SHA256"
      credentialScope =
        BS.intercalate
          "/"
          [dateStamp, TE.encodeUtf8 region, TE.encodeUtf8 service, "aws4_request"]
      stringToSign =
        BS.intercalate
          "\n"
          [ algorithm,
            amzDate,
            credentialScope,
            hashHex canonicalRequest
          ]

      -- Calculate signature
      kDate = hmacSHA256 ("AWS4" <> secretKey) dateStamp
      kRegion = hmacSHA256 kDate (TE.encodeUtf8 region)
      kService = hmacSHA256 kRegion (TE.encodeUtf8 service)
      kSigning = hmacSHA256 kService "aws4_request"
      signature = Base16.encode $ hmacSHA256 kSigning stringToSign

      -- Authorization header
      authHeader =
        algorithm <> " "
          <> "Credential="
          <> accessKey
          <> "/"
          <> credentialScope
          <> ", "
          <> "SignedHeaders="
          <> signedHeaders
          <> ", "
          <> "Signature="
          <> signature

      finalHeaders =
        (CI.mk "Authorization", authHeader)
          : [(CI.mk k, v) | (k, v) <- allHeaders]

  pure finalHeaders

-- | HMAC-SHA256
hmacSHA256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSHA256 key msg = convert (hmac key msg :: HMAC SHA256)

-- | SHA256 hash as hex string
hashHex :: BS.ByteString -> BS.ByteString
hashHex = Base16.encode . convert . hashWith SHA256
