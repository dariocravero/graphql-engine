-- | Log redaction functionality for filtering sensitive data from logs.
--
-- This module provides types and functions to redact sensitive field values
-- from JSON log output. Field matching is case-insensitive and ignores
-- underscores and hyphens (e.g., "password" matches "Password", "pass_word", "PASS-WORD").
module Hasura.Logging.Redaction
  ( -- * Configuration Types
    RedactedLogFields (..),
    RedactionConfig (..),

    -- * Building Configuration
    mkRedactionConfig,

    -- * Redaction Functions
    redactJSON,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Data.Vector qualified as V
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | Raw list of field names to redact, as parsed from the environment variable.
-- This is the user-facing configuration type.
newtype RedactedLogFields = RedactedLogFields {unRedactedLogFields :: [Text]}
  deriving (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

instance NFData RedactedLogFields

instance Hashable RedactedLogFields

instance J.FromJSON RedactedLogFields where
  parseJSON = J.withArray "RedactedLogFields" $ \arr ->
    RedactedLogFields <$> mapM J.parseJSON (V.toList arr)

instance J.ToJSON RedactedLogFields where
  toJSON (RedactedLogFields fields) = J.toJSON fields

--------------------------------------------------------------------------------

-- | Pre-compiled redaction configuration for efficient runtime use.
-- The field names are normalized (lowercase, no separators) for fast matching.
data RedactionConfig = RedactionConfig
  { -- | The set of normalized field names (lowercase, no underscores/hyphens)
    rcNormalizedFields :: !(HashSet.HashSet Text),
    -- | Quick check: is redaction enabled at all?
    rcEnabled :: !Bool
  }
  deriving (Show, Eq)

-- | The placeholder value used for redacted fields.
redactedValue :: J.Value
redactedValue = J.String "[REDACTED]"

--------------------------------------------------------------------------------

-- | Normalize a field name for comparison by:
--   1. Converting to lowercase
--   2. Removing all underscores and hyphens
--
-- Examples:
--   - "password"   -> "password"
--   - "Password"   -> "password"
--   - "pass_word"  -> "password"
--   - "PASS-WORD"  -> "password"
--   - "API_Key"    -> "apikey"
normalizeFieldName :: Text -> Text
normalizeFieldName = T.filter (\c -> c /= '_' && c /= '-') . T.toLower

-- | Build a 'RedactionConfig' from 'RedactedLogFields'.
-- This normalizes all field names and stores them in a 'HashSet' for O(1) lookup.
-- Should be called once at startup.
mkRedactionConfig :: RedactedLogFields -> RedactionConfig
mkRedactionConfig (RedactedLogFields fields) =
  let -- Filter out empty/whitespace-only entries and normalize
      normalizedFields =
        HashSet.fromList
          . filter (not . T.null)
          . map normalizeFieldName
          $ fields
      enabled = not (HashSet.null normalizedFields)
   in RedactionConfig
        { rcNormalizedFields = normalizedFields,
          rcEnabled = enabled
        }

-- | Check if a field name should be redacted.
-- Uses normalized comparison for case-insensitive, separator-agnostic matching.
shouldRedactField :: RedactionConfig -> Text -> Bool
shouldRedactField config fieldName =
  HashSet.member (normalizeFieldName fieldName) (rcNormalizedFields config)

--------------------------------------------------------------------------------

-- | Recursively redact sensitive values from a JSON 'J.Value'.
--
-- Only values are redacted, never keys - this preserves log structure.
-- The function walks the JSON structure:
--   - For Objects: check each key against the redaction list, redact matching values
--   - For Arrays: recursively process each element
--   - For other values (String, Number, Bool, Null): return unchanged
--
-- If redaction is disabled ('rcEnabled' is False), returns the value unchanged.
redactJSON :: RedactionConfig -> J.Value -> J.Value
redactJSON config
  | not (rcEnabled config) = id -- Fast path: no redaction configured
  | otherwise = go
  where
    go :: J.Value -> J.Value
    go val = case val of
      J.Object obj -> J.Object (redactObject obj)
      J.Array arr -> J.Array (V.map go arr)
      -- Scalars pass through unchanged
      other -> other

    redactObject :: J.Object -> J.Object
    redactObject = KM.mapWithKey redactField

    redactField :: K.Key -> J.Value -> J.Value
    redactField k v
      | shouldRedactField config (K.toText k) = redactScalarOnly v
      | otherwise = go v -- Recurse into non-matching fields

    -- Only redact scalar values; for objects/arrays under a sensitive key,
    -- recurse and redact nested sensitive fields
    redactScalarOnly :: J.Value -> J.Value
    redactScalarOnly v = case v of
      J.Object obj -> J.Object (redactObject obj)
      J.Array arr -> J.Array (V.map go arr)
      -- Redact scalar values
      _ -> redactedValue
