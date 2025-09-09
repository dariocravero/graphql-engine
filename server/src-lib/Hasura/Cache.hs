{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hasura Enterprise-compatible caching implementation for open-source GraphQL Engine
module Hasura.Cache
  ( -- * Cache Types
    HasuraCache (..),
    CacheResult (..),
    
    -- * Cache Operations  
    initializeCache,
    shutdownCache,
    lookupQuery,
    storeQuery,
    
    -- * Re-exports
    module Hasura.Cache.Config,
    module Hasura.Cache.Redis,
  ) where

import Data.Environment qualified as Env
import Data.Text qualified as T
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasura.Authentication.User (UserInfo, _uiRole, _uiSession)
import Hasura.Authentication.Session (SessionVariable, sessionVariablesToHeaders, filterSessionVariables, fromSessionVariable)
import Hasura.Cache.Config
import Hasura.Cache.Redis
import Hasura.EncJSON (EncJSON)
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqParsed, GQLReq(..), VariableValues, GQLExecDoc(..))
import Crypto.Hash qualified as Hash
import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as TE
import Hasura.Prelude
import Network.HTTP.Types (Header)
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS

-- | Cache result type
data CacheResult
  = CacheHit EncJSON
  | CacheMiss
  | CacheError Text

instance Show CacheResult where
  show (CacheHit _) = "CacheHit <EncJSON>"
  show CacheMiss = "CacheMiss"
  show (CacheError err) = "CacheError " <> T.unpack err

instance Eq CacheResult where
  CacheMiss == CacheMiss = True
  (CacheError err1) == (CacheError err2) = err1 == err2
  _ == _ = False -- CacheHit can't be compared due to EncJSON

-- | Main cache instance
data HasuraCache = HasuraCache
  { hcRedisCache :: RedisCache,
    hcConfig :: CacheConfig
  }

-- | Initialize cache from environment
initializeCache :: (MonadIO m) => Env.Environment -> m (Either String (Maybe HasuraCache))
initializeCache env = liftIO $ do
  case readCacheConfigFromEnv env of
    Left err -> pure $ Left $ "Failed to read cache config: " <> err
    Right rawConfig -> do
      case processCacheConfig rawConfig of
        Left err -> pure $ Left $ "Failed to process cache config: " <> err
        Right Nothing -> pure $ Right Nothing  -- Caching disabled
        Right (Just config) -> do
          redisResult <- initRedisCache config
          case redisResult of
            Left err -> pure $ Left err
            Right redisCache -> pure $ Right $ Just $ HasuraCache redisCache config

-- | Shutdown cache
shutdownCache :: (MonadIO m) => HasuraCache -> m ()
shutdownCache HasuraCache{..} = closeRedisCache hcRedisCache

-- | Look up query in cache with permission-specific session variables
lookupQuery :: (MonadIO m) => HasuraCache -> GQLReqParsed -> UserInfo -> HashSet.HashSet SessionVariable -> [Header] -> m CacheResult
lookupQuery HasuraCache{..} req userInfo usedSessionVars headers = do
  let components = extractCacheKeyComponents req userInfo usedSessionVars headers
      cacheKey = generateCacheKey components
  
  -- Store debug info for every lookup
  _ <- storeDebugInfo hcRedisCache cacheKey components
      
  result <- getCached hcRedisCache cacheKey
  case result of
    Left err -> pure $ CacheError err
    Right Nothing -> pure CacheMiss
    Right (Just cachedData) -> do
      -- TODO: Add cache entry size and rate limiting checks here
      pure $ CacheHit cachedData

-- | Store query result in cache with permission-specific session variables
storeQuery :: (MonadIO m) => HasuraCache -> GQLReqParsed -> UserInfo -> HashSet.HashSet SessionVariable -> [Header] -> EncJSON -> m (Either Text ())
storeQuery HasuraCache{..} req userInfo usedSessionVars headers response = do
  let components = extractCacheKeyComponents req userInfo usedSessionVars headers
      cacheKey = generateCacheKey components
      CacheConfig{..} = hcConfig
      
  -- TODO: Add cache entry size validation here
  -- For now, just use the max TTL from config
  setCached hcRedisCache cacheKey response ccMaxEntryTTL

-- | Extract cache key components from request
extractCacheKeyComponents :: GQLReqParsed -> UserInfo -> HashSet.HashSet SessionVariable -> [Header] -> CacheKeyComponents
extractCacheKeyComponents req userInfo usedSessionVars headers = CacheKeyComponents
  { ckcQuery = serializeQuery $ _grQuery req,  -- Serialize the actual query
    ckcVariables = fmap serializeVariables (_grVariables req),
    ckcOperationName = fmap (\name -> T.pack $ show name) (_grOperationName req),
    ckcUserRole = T.pack $ show $ _uiRole userInfo,  -- Convert role to text
    ckcHeaders = extractUsedSessionVariables userInfo usedSessionVars headers
  }
  where
    -- | Serialize variables to text for cache key
    serializeVariables :: VariableValues -> Text
    serializeVariables vars = 
      let sorted = sort $ HashMap.toList vars -- Sort for consistent ordering
          encoded = map (\(k, v) -> T.pack (show k) <> "=" <> TE.decodeUtf8 (LBS.toStrict $ J.encode v)) sorted
      in T.intercalate "&" encoded
      
    -- | Create a stable hash-based representation of the GraphQL query for caching
    -- This approach ensures identical queries produce identical cache keys
    serializeQuery :: GQLExecDoc -> Text
    serializeQuery (GQLExecDoc execDefs) = 
      let -- Convert to string representation
          queryStr = T.pack $ show execDefs
          -- Create a stable hash of the query
          queryHash = Hash.hash (TE.encodeUtf8 queryStr) :: Hash.Digest Hash.SHA256
      in T.pack $ show queryHash
    
    -- | Extract only the session variables that are used in permissions
    extractUsedSessionVariables :: UserInfo -> HashSet.HashSet SessionVariable -> [Header] -> [Header]
    extractUsedSessionVariables uInfo usedVars extraHeaders = 
      let -- Get session variables from UserInfo
          sessionVars = _uiSession uInfo
          -- Filter to only the ones used in permissions
          filteredSessionVars = filterSessionVariables (\var _ -> HashSet.member var usedVars) sessionVars
          -- Convert to headers
          sessionHeaders = sessionVariablesToHeaders filteredSessionVars
          -- Also check if any used session variables are in the extra headers
          relevantExtraHeaders = filter (\(name, _) -> 
            let nameText = TE.decodeUtf8 $ CI.original name
                lowerName = T.toLower nameText
            in T.isPrefixOf "x-hasura-" lowerName && 
               lowerName /= "x-hasura-admin-secret" &&
               any (\var -> lowerName == T.toLower (fromSessionVariable var)) (HashSet.toList usedVars)
            ) extraHeaders
      in sort $ sessionHeaders ++ relevantExtraHeaders
    
