# Hasura GraphQL Engine Caching Implementation

This document describes the enterprise-compatible caching implementation that has been added to the open-source GraphQL Engine.

## Overview

The caching implementation provides Redis-based response caching for GraphQL queries, matching the functionality described in [Hasura's Enterprise Caching documentation](https://hasura.io/docs/2.0/caching/enterprise-caching/).

## Features

- **Redis-based caching**: Uses Redis as the cache backend for scalability and performance
- **Configurable TTL**: Set maximum time-to-live for cache entries (default: 3600 seconds)
- **Entry size limits**: Control maximum cacheable response size (default: 1000 MB)
- **Rate limiting**: Configure cache bucket rate and size for throughput control
- **Cache key generation**: Intelligent cache key generation based on query, variables, user role, and sanitized headers
- **Error handling**: Graceful fallback when cache is unavailable

## Configuration

### Environment Variables

The caching system is configured using the following environment variables:

#### Required
- `HASURA_GRAPHQL_REDIS_URL`: Redis connection URL (e.g., `redis://localhost:6379`)
  - If not set, caching is disabled

#### Optional
- `HASURA_GRAPHQL_CACHE_MAX_ENTRY_TTL`: Maximum cache entry TTL in seconds (default: 3600)
- `HASURA_GRAPHQL_CACHE_MAX_ENTRY_SIZE`: Maximum cacheable response size in MB (default: 1000)
- `HASURA_GRAPHQL_CACHE_BUCKET_RATE`: Cache token bucket recharge rate in MB/s (default: 10)
- `HASURA_GRAPHQL_CACHE_BUCKET_SIZE`: Maximum cache store size in MB (default: 1000)

### Example Configuration

```bash
export HASURA_GRAPHQL_REDIS_URL="redis://localhost:6379"
export HASURA_GRAPHQL_CACHE_MAX_ENTRY_TTL="7200"
export HASURA_GRAPHQL_CACHE_MAX_ENTRY_SIZE="500"
export HASURA_GRAPHQL_CACHE_BUCKET_RATE="20"
export HASURA_GRAPHQL_CACHE_BUCKET_SIZE="2000"
```

## Architecture

The caching implementation consists of several modules:

### Core Modules

1. **`Hasura.Cache.Config`**: Configuration parsing and validation
2. **`Hasura.Cache.Redis`**: Redis client integration and cache operations
3. **`Hasura.Cache`**: Main cache interface and orchestration

### Integration Points

1. **`Hasura.App.State`**: Added `appEnvCache` field to `AppEnv`
2. **`Hasura.App`**: Cache initialization and `MonadExecuteQuery` implementation

## Cache Key Generation

Cache keys are generated based on the following components:

- GraphQL query text
- Variable values
- Operation name
- User role
- Session variables used in permission rules (see Session Variables section below)

The cache key is hashed using SHA256 for consistent key length and security.

### Session Variables in Cache Keys

**Important**: The caching implementation has been updated to support the Hasura Enterprise caching specification, which requires that only session variables actually used in permission rules should be included in cache keys.

**Current Status**: The infrastructure is in place, but the full implementation is pending. Currently, all session variables are included in cache keys (backward compatible behavior).

**Future Behavior**: Only session variables referenced in permission boolean expressions (e.g., `x-hasura-user-id` in a row-level permission like `{"user_id": {"_eq": "X-Hasura-User-Id"}}`) will be included in the cache key. This will significantly improve cache hit rates for queries where unused session variables vary between requests.

## Implementation Details

### Cache Lifecycle

1. **Initialization**: Cache is initialized during application startup if Redis URL is configured
2. **Query Processing**: For each GraphQL query:
   - Generate cache key from query components
   - Check Redis for cached response
   - If cache hit: return cached response
   - If cache miss: execute query and store result with TTL
3. **Shutdown**: Clean disconnection from Redis

### Error Handling

The implementation includes robust error handling:

- Redis connection failures: Log error and continue without caching
- Cache lookup failures: Continue with query execution
- Cache store failures: Log error but don't fail the query

### Security Considerations

- Sensitive headers (authorization, cookies, admin secrets) are excluded from cache key generation
- Cache keys are hashed to prevent information leakage
- User roles are included in cache keys to prevent cross-user data leakage

## Testing

### Manual Testing

1. **Start Redis**:
   ```bash
   docker run -d -p 6379:6379 redis:alpine
   ```

2. **Configure Hasura**:
   ```bash
   export HASURA_GRAPHQL_REDIS_URL="redis://localhost:6379"
   ```

3. **Run GraphQL Engine**:
   ```bash
   cabal run graphql-engine -- serve
   ```

4. **Test Caching**:
   - Execute a GraphQL query twice
   - The second execution should be faster due to caching
   - Check Redis for cached data: `redis-cli keys "cache:*"`

### Automated Testing

The implementation includes unit tests for:
- Cache configuration parsing
- Cache key generation
- Redis integration
- Error handling scenarios

## Monitoring

### Metrics

The cache implementation provides metrics for monitoring:
- Cache hit/miss ratios
- Cache operation latencies
- Error rates

### Logging

Cache operations are logged at appropriate levels:
- Info: Cache initialization and configuration
- Debug: Cache hits/misses
- Error: Redis connection issues and failures

## Troubleshooting

### Common Issues

1. **Cache not working**:
   - Check `HASURA_GRAPHQL_REDIS_URL` is set correctly
   - Verify Redis is accessible from Hasura
   - Check logs for connection errors

2. **High cache miss rate**:
   - Verify query structure is consistent
   - Check if variables or headers are changing unnecessarily
   - Monitor cache TTL settings

3. **Redis connection errors**:
   - Verify Redis server is running
   - Check network connectivity
   - Validate Redis URL format

### Debug Commands

```bash
# Check Redis connectivity
redis-cli ping

# View cached keys
redis-cli keys "cache:*"

# Monitor Redis operations
redis-cli monitor

# Get cache statistics
redis-cli info stats
```

## Compliance with Enterprise Edition

This implementation is designed to be compatible with Hasura's Enterprise Edition caching:

- Uses the same environment variable names
- Implements the same configuration options
- Provides equivalent functionality
- Maintains the same API behavior

## Future Enhancements

Potential improvements for the caching system:

1. **Cache warming**: Pre-populate cache with common queries
2. **Intelligent invalidation**: Invalidate cache entries based on data changes
3. **Cache analytics**: Detailed metrics and reporting
4. **Multi-tier caching**: In-memory + Redis caching layers
5. **Cache compression**: Compress large responses before storing

## Contributing

When contributing to the caching implementation:

1. Ensure compatibility with existing Enterprise Edition behavior
2. Add appropriate tests for new functionality
3. Update documentation for any configuration changes
4. Follow existing error handling patterns
5. Maintain security best practices