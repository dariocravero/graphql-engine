{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Hasura.Cache.Config
  ( -- * Configuration
    CacheConfig (..),
    defaultCacheConfig,
    
    -- * Environment Variable Processing
    readCacheConfigFromEnv,
    
    -- * Types
    CacheConfigRaw (..),
    
    -- * Utils
    processCacheConfig
  ) where

import Data.Environment qualified as Env
import Data.Text qualified as T
import Data.Word (Word32)
import Hasura.Prelude

-- | Raw configuration values from environment variables
data CacheConfigRaw = CacheConfigRaw
  { ccrRedisUrl :: Maybe Text,
    ccrMaxEntryTTL :: Maybe Word32,
    ccrMaxEntrySize :: Maybe Word64,  -- in bytes
    ccrBucketRate :: Maybe Word64,    -- in MB/s
    ccrBucketSize :: Maybe Word64     -- in MB
  }
  deriving stock (Show, Eq, Generic)

-- | Processed cache configuration
data CacheConfig = CacheConfig
  { ccRedisUrl :: Text,
    ccMaxEntryTTL :: Word32,          -- in seconds
    ccMaxEntrySize :: Word64,         -- in bytes
    ccBucketRate :: Word64,           -- in MB/s
    ccBucketSize :: Word64            -- in MB
  }
  deriving stock (Show, Eq, Generic)

-- | Default cache configuration based on Hasura Enterprise defaults
defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig
  { ccRedisUrl = "redis://localhost:6379",
    ccMaxEntryTTL = 3600,           -- 1 hour
    ccMaxEntrySize = 1000 * 1024 * 1024,  -- 1000 MB
    ccBucketRate = 10,              -- 10 MB/s
    ccBucketSize = 1000             -- 1 GB
  }

-- | Read cache configuration from environment variables
readCacheConfigFromEnv :: Env.Environment -> Either String CacheConfigRaw
readCacheConfigFromEnv env = do
  let
    ccrRedisUrl = T.pack <$> Env.lookupEnv env "HASURA_GRAPHQL_REDIS_URL"
    ccrMaxEntryTTL = parseEnvVar "HASURA_GRAPHQL_CACHE_MAX_ENTRY_TTL" env
    ccrMaxEntrySize = parseEnvVar "HASURA_GRAPHQL_CACHE_MAX_ENTRY_SIZE" env
    ccrBucketRate = parseEnvVar "HASURA_GRAPHQL_CACHE_BUCKET_RATE" env  
    ccrBucketSize = parseEnvVar "HASURA_GRAPHQL_CACHE_BUCKET_SIZE" env
    
  pure CacheConfigRaw{..}

-- | Process raw cache configuration into final config with defaults
processCacheConfig :: CacheConfigRaw -> Either String (Maybe CacheConfig)
processCacheConfig CacheConfigRaw{..} = do
  case ccrRedisUrl of
    Nothing -> pure Nothing  -- No Redis URL means caching is disabled
    Just redisUrl -> do
      let config = CacheConfig
            { ccRedisUrl = redisUrl,
              ccMaxEntryTTL = fromMaybe (ccMaxEntryTTL defaultCacheConfig) ccrMaxEntryTTL,
              ccMaxEntrySize = mbToBytes $ fromMaybe (bytesToMb $ ccMaxEntrySize defaultCacheConfig) ccrMaxEntrySize,
              ccBucketRate = fromMaybe (ccBucketRate defaultCacheConfig) ccrBucketRate,
              ccBucketSize = fromMaybe (ccBucketSize defaultCacheConfig) ccrBucketSize
            }
      pure $ Just config

-- Helper functions
parseEnvVar :: (Read a) => String -> Env.Environment -> Maybe a
parseEnvVar key env = Env.lookupEnv env key >>= readMaybe

mbToBytes :: Word64 -> Word64
mbToBytes mb = mb * 1024 * 1024

bytesToMb :: Word64 -> Word64
bytesToMb bytes = bytes `div` (1024 * 1024)