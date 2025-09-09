{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.Cache.Redis
  ( -- * Redis Cache Implementation
    RedisCache (..),
    initRedisCache,
    closeRedisCache,
    
    -- * Cache Operations
    getCached,
    setCached,
    storeDebugInfo,
    
    -- * Cache Key Generation
    generateCacheKey,
    
    -- * Types
    CacheKeyComponents (..),
  ) where

import Control.Exception (try, SomeException)
import Crypto.Hash (SHA256, Digest)
import Crypto.Hash qualified as Hash
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word32)
import Database.Redis qualified as Redis
import Hasura.Cache.Config (CacheConfig (..))
import Hasura.EncJSON (EncJSON, encJFromLBS, encJToLBS)
import Hasura.Prelude
import Network.HTTP.Types (Header)

-- | Components used for cache key generation
data CacheKeyComponents = CacheKeyComponents
  { ckcQuery :: Text,
    ckcVariables :: Maybe Text, -- Serialized variables
    ckcOperationName :: Maybe Text,
    ckcUserRole :: Text,
    ckcHeaders :: [Header]
  }
  deriving (Show, Eq)

-- | Redis cache instance
data RedisCache = RedisCache
  { rcConnection :: Redis.Connection,
    rcConfig :: CacheConfig
  }

-- | Initialize Redis cache connection
initRedisCache :: (MonadIO m) => CacheConfig -> m (Either String RedisCache)
initRedisCache config@CacheConfig{..} = liftIO $ do
  connResult <- try $ do
    case Redis.parseConnectInfo (T.unpack ccRedisUrl) of
      Left err -> error err
      Right connInfo -> Redis.connect connInfo
  case connResult of
    Left (e :: SomeException) -> pure $ Left $ "Failed to connect to Redis: " <> show e
    Right conn -> do
      -- Test the connection
      pingResult <- Redis.runRedis conn Redis.ping
      case pingResult of
        Left e -> pure $ Left $ "Redis connection test failed: " <> show e
        Right Redis.Pong -> pure $ Right $ RedisCache conn config
        Right _ -> pure $ Left "Unexpected Redis ping response"

-- | Close Redis cache connection
closeRedisCache :: (MonadIO m) => RedisCache -> m ()
closeRedisCache RedisCache{..} = liftIO $ Redis.disconnect rcConnection

-- | Get cached response
getCached :: (MonadIO m) => RedisCache -> BS.ByteString -> m (Either Text (Maybe EncJSON))
getCached RedisCache{..} key = liftIO $ do
  result <- Redis.runRedis rcConnection $ Redis.get key
  case result of
    Left e -> pure $ Left $ "Redis GET error: " <> T.pack (show e)
    Right Nothing -> pure $ Right Nothing
    Right (Just cachedData) -> do
      case J.decode $ LBS.fromStrict cachedData of
        Nothing -> pure $ Left "Failed to decode cached data"
        Just (CachedResponse _ jsonData) -> do
          pure $ Right $ Just $ encJFromLBS jsonData

-- | Set cached response with TTL
setCached :: (MonadIO m) => RedisCache -> BS.ByteString -> EncJSON -> Word32 -> m (Either Text ())
setCached RedisCache{..} key response ttl = liftIO $ do
  now <- getCurrentTime
  let cachedResp = CachedResponse now (encJToLBS response)
  let encodedData = LBS.toStrict $ J.encode cachedResp
  
  result <- Redis.runRedis rcConnection $ Redis.setex key (fromIntegral ttl) encodedData
  case result of
    Left e -> pure $ Left $ "Redis SET error: " <> T.pack (show e)
    Right Redis.Ok -> pure $ Right ()
    Right _ -> pure $ Left "Unexpected Redis SET response"

-- | Store debug info about cache key components
storeDebugInfo :: (MonadIO m) => RedisCache -> BS.ByteString -> CacheKeyComponents -> m ()
storeDebugInfo RedisCache{..} key CacheKeyComponents{..} = liftIO $ do
  let debugKey = key <> ":debug"
      debugData = J.object
        [ "query" J..= ckcQuery,
          "variables" J..= fromMaybe "<none>" ckcVariables,
          "operationName" J..= fromMaybe "<none>" ckcOperationName,
          "userRole" J..= ckcUserRole,
          "headers" J..= map (\(h,v) -> TE.decodeUtf8 (CI.original h) <> ":" <> TE.decodeUtf8 v) ckcHeaders
        ]
  _ <- Redis.runRedis rcConnection $ Redis.setex debugKey 300 (LBS.toStrict $ J.encode debugData)
  pure ()

-- | Generate cache key from query components
generateCacheKey :: CacheKeyComponents -> BS.ByteString
generateCacheKey CacheKeyComponents{..} = 
  let sortedHeaders = sort $ map headerToText ckcHeaders -- Sort headers for consistency
      headerText = T.intercalate "," sortedHeaders
      keyData = T.intercalate "|" 
        [ "hasura:cache",
          ckcQuery,
          fromMaybe "" ckcVariables,
          fromMaybe "" ckcOperationName,
          ckcUserRole,
          headerText
        ]
      keyBytes = TE.encodeUtf8 keyData
      keyHash = Hash.hash keyBytes :: Digest SHA256
  in "cache:" <> C8.pack (show keyHash)
  where
    headerToText (h, v) = TE.decodeUtf8 (CI.original h) <> ":" <> TE.decodeUtf8 v


-- | Cached response with timestamp
data CachedResponse = CachedResponse
  { crTimestamp :: UTCTime,
    crData :: LBS.ByteString
  }
  deriving (Show, Eq)

instance J.ToJSON CachedResponse where
  toJSON CachedResponse{..} = J.object
    [ "timestamp" J..= crTimestamp,
      "data" J..= TE.decodeUtf8 (LBS.toStrict crData)
    ]

instance J.FromJSON CachedResponse where
  parseJSON = J.withObject "CachedResponse" $ \o -> do
    timestamp <- o J..: "timestamp"
    dataText <- o J..: "data"
    pure $ CachedResponse timestamp (LBS.fromStrict $ TE.encodeUtf8 dataText)