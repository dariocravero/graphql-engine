# Testing Cache Session Variables Fix

## Current Status

The caching implementation has been updated with the infrastructure to support caching based only on session variables used in permissions. However, the actual extraction of used session variables from the execution plan is still TODO.

### What was implemented:

1. **Function to extract session variables from boolean expressions** (`getSessionVariablesFromAnnBoolExpPartialSQL` in `Hasura.RQL.IR.BoolExp`)
   - Can extract session variables from permission boolean expressions
   - Handles all boolean expression types including AND, OR, NOT, EXISTS
   - Properly traverses nested structures

2. **Modified cache key generation** (`extractCacheKeyComponents` in `Hasura.Cache`)
   - Now accepts a set of used session variables
   - Only includes those session variables in the cache key
   - Falls back to all session variables when empty set is provided

3. **Updated MonadExecuteQuery instance** (in `Hasura.App`)
   - Passes session variables to cache operations
   - Currently passes empty set (TODO: extract from execution plan)

## Testing the Current Behavior

Currently, the implementation passes an empty set of used session variables, which means it will use ALL session variables (maintaining the current behavior). This ensures backward compatibility while the infrastructure is in place.

To fully implement the optimization:
1. Need to extract permission boolean expressions from the execution plan
2. Apply `getSessionVariablesFromAnnBoolExpPartialSQL` to those expressions
3. Combine all used variables into a set
4. Pass this set to the cache operations

## Manual Testing Steps

1. **Start Redis**:
   ```bash
   docker run -d -p 6379:6379 redis:alpine
   ```

2. **Configure Hasura with caching**:
   ```bash
   export HASURA_GRAPHQL_REDIS_URL="redis://localhost:6379"
   ```

3. **Run GraphQL Engine**:
   ```bash
   cabal run graphql-engine -- serve --database-url "postgresql://user:pass@localhost:5432/hasura"
   ```

4. **Test queries with @cached directive**:
   - Create a table with row-level permissions using session variables
   - Execute queries with different session variable combinations
   - Currently: separate cache entries for each unique combination of ALL session variables
   - Future: separate cache entries only for different values of USED session variables

## Example Permission

```json
{
  "user_id": {
    "_eq": "X-Hasura-User-Id"
  }
}
```

With this permission, only `X-Hasura-User-Id` should affect the cache key, not other session variables like `X-Hasura-Org-Id` or `X-Hasura-Role`.