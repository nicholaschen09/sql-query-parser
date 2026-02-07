# Go SQL Parser

A Go implementation of the SQL query parser, providing a REST API endpoint for parsing and executing SQL queries.

## Setup

1. Make sure you have Go 1.21+ installed
2. Install dependencies:
```bash
cd go
go mod tidy
```

## Running the Server

```bash
go run cmd/server/main.go
```

Or build and run:
```bash
go build -o bin/server cmd/server/main.go
./bin/server
```

The server will start on port 8080 (or the port specified in the `PORT` environment variable).

## API Endpoints

### POST /execute

Execute a SQL query against provided data.

**Request Body:**
```json
{
  "query": "SELECT state, pop FROM table WHERE pop > 5000",
  "data": [
    { "state": "California", "region": "West", "pop": 10000 },
    { "state": "Texas", "region": "South", "pop": 5000 }
  ]
}
```

**Response:**
```json
{
  "success": true,
  "data": [
    { "state": "California", "pop": 10000 }
  ]
}
```

### GET /health

Health check endpoint.

**Response:**
```json
{
  "status": "ok"
}
```

## Testing

### Unit Tests

Run parser unit tests:
```bash
make test
# or
go test ./...
```

Run with verbose output:
```bash
make test-verbose
# or
go test -v ./...
```

Run with coverage:
```bash
make test-coverage
# or
go test -cover ./...
```

### API Testing

1. **Start the server** (in one terminal):
```bash
make run
# or
go run cmd/server/main.go
```

2. **Test the API** (in another terminal):
```bash
# Using the test script (requires jq)
make test-api
# or
bash test-api.sh

# Or manually with curl
curl http://localhost:8080/health

curl -X POST http://localhost:8080/execute \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT state, pop FROM table WHERE pop > 5000",
    "data": [
      {"state": "California", "region": "West", "pop": 10000},
      {"state": "Texas", "region": "South", "pop": 5000}
    ]
  }'
```

See [TESTING.md](TESTING.md) for comprehensive testing instructions.

## Integration with Frontend

The frontend can call this API instead of using the TypeScript parser. See the frontend code for the integration option.
