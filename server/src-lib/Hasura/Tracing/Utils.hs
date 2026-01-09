-- | This module contains a collection of utility functions we use with tracing
-- throughout the codebase, but that are not a core part of the library. If we
-- were to move tracing to a separate library, those functions should be kept
-- here in the core engine code.
module Hasura.Tracing.Utils
  ( traceHTTPRequest,
    traceHTTPRequestWithLambda,
    attachSourceConfigAttributes,
    composedPropagator,
  )
where

import Control.Lens
import Data.Environment qualified as Env
import Data.String
import Data.Text.Extended (toTxt)
import Hasura.Eventing.Lambda (transformLambdaRequest)
import Hasura.Prelude
import Hasura.RQL.Types.SourceConfiguration (HasSourceConfiguration (..))
import Hasura.Tracing.Class
import Hasura.Tracing.Context
import Hasura.Tracing.Propagator
import Hasura.Tracing.Propagator.B3 (b3TraceContextPropagator)
import Hasura.Tracing.Propagator.W3CTraceContext (w3cTraceContextPropagator)
import Hasura.Tracing.TraceId (SpanKind (SKClient))
import Network.HTTP.Client.Transformable qualified as HTTP

-- | Wrap the execution of an HTTP request in a span in the current
-- trace. Despite its name, this function does not start a new trace, and the
-- span will therefore not be recorded if the surrounding context isn't traced
-- (see 'spanWith').
--
-- Additionally, this function adds metadata regarding the request to the
-- created span, and injects the trace context into the HTTP header.
traceHTTPRequest ::
  (MonadIO m, MonadTrace m) =>
  HttpPropagator ->
  -- | http request that needs to be made
  HTTP.Request ->
  -- | a function that takes the traced request and executes it
  (HTTP.Request -> m a) ->
  m a
traceHTTPRequest propagator req f = do
  let method = bsToTxt (view HTTP.method req)
      uri = view HTTP.url req
  newSpan (method <> " " <> uri) SKClient do
    let reqBytes = HTTP.getReqSize req
    attachMetadata [("request_body_bytes", fromString (show reqBytes))]
    headers <- fmap (maybe [] toHeaders) currentContext
    f $ over HTTP.headers (headers <>) req
  where
    toHeaders :: TraceContext -> [HTTP.Header]
    toHeaders context = inject propagator context []

-- | Like 'traceHTTPRequest', but handles Lambda URL transformation.
--
-- If the request URL starts with @aws://@ scheme, it is transparently transformed
-- to a signed HTTPS request to the AWS Lambda Invoke API before execution.
-- Otherwise the request is passed through unchanged.
--
-- Note: Lambda function errors (indicated by @X-Amz-Function-Error@ header) are
-- handled at the HTTP response parsing layer (see 'runHTTP' in Eventing.HTTP).
traceHTTPRequestWithLambda ::
  (MonadIO m, MonadTrace m) =>
  Env.Environment ->
  HttpPropagator ->
  -- | HTTP request (may have aws:// URL for Lambda)
  HTTP.Request ->
  -- | Function that executes the request
  (HTTP.Request -> m a) ->
  m a
traceHTTPRequestWithLambda env propagator req f = do
  -- Transform Lambda URLs (aws://) to signed HTTPS requests
  transformedReq <- liftIO $ transformLambdaRequest env req
  -- Execute with tracing (the transformed request is used for both tracing metadata and execution)
  traceHTTPRequest propagator transformedReq f

attachSourceConfigAttributes :: forall b m. (HasSourceConfiguration b, MonadTrace m) => SourceConfig b -> m ()
attachSourceConfigAttributes sourceConfig = do
  let backendSourceKind = sourceConfigBackendSourceKind @b sourceConfig
  attachMetadata [("source.kind", toTxt $ backendSourceKind)]

-- | Propagator composition of Trace Context and ZipKin B3.
composedPropagator :: HttpPropagator
composedPropagator = b3TraceContextPropagator <> w3cTraceContextPropagator
