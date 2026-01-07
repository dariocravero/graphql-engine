On Mac with `Dockerfile.build`:

```sh
docker buildx build \        
  --platform linux/arm64 \
  -f Dockerfile.build \
  -t graphql-engine:v2.48.9 \
  --load \
  .
```