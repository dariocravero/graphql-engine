# Read Replica Support Test

This directory contains a test setup to demonstrate read replica support in Hasura Community Edition.

## Overview

With this implementation, Hasura CE can now route read queries to read replicas while sending all write operations (mutations, metadata operations) to the primary database.

## How it Works

1. **Query Routing**: 
   - GraphQL queries and subscriptions are routed to read replicas (randomly selected)
   - GraphQL mutations are routed to the primary database
   - Metadata operations are routed to the primary database

2. **Configuration**:
   - Read replicas are configured in the source metadata
   - Each replica can have its own pool settings
   - The system randomly selects a read replica for each read operation

## Setup

1. Set up your primary and replica PostgreSQL databases
2. Set the following environment variables:
   ```bash
   export PRIMARY_DATABASE_URL="postgresql://user:password@primary-host:5432/mydb"
   export REPLICA1_DATABASE_URL="postgresql://user:password@replica1-host:5432/mydb"
   export REPLICA2_DATABASE_URL="postgresql://user:password@replica2-host:5432/mydb"
   ```

3. Apply the metadata configuration:
   ```bash
   hasura metadata apply
   ```

## Testing

You can verify that queries are being routed to read replicas by:

1. Monitoring the connection logs on your replica databases
2. Running queries and checking which database handles them
3. Running mutations and verifying they only go to the primary

## Implementation Details

The implementation modifies:
- `PGExecCtx` to manage multiple connection pools
- `mkPGExecCtxWithReadReplicas` to create an execution context with replica support
- Query routing logic to send read operations to replicas
- Source configuration to parse and create replica pools