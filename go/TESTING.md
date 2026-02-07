# Testing the Go SQL Parser

This guide covers all the ways to test the Go parser implementation.

## 1. Unit Tests (Parser Logic)

Run the parser unit tests:

```bash
cd go
go test ./...
```

Run with verbose output:
```bash
go test -v ./...
```

Run a specific test:
```bash
go test -v -run TestSQLParser
```

Run with coverage:
```bash
go test -cover ./...
```

## 2. Manual API Testing

### Start the Server

```bash
cd go
go run cmd/server/main.go
```

The server will start on `http://localhost:8080` (or the port specified in `PORT` env var).

### Test Health Endpoint

```bash
curl http://localhost:8080/health
```

Expected response:
```json
{"status":"ok"}
```

### Test Execute Endpoint

```bash
curl -X POST http://localhost:8080/execute \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT state, pop FROM table WHERE pop > 5000",
    "data": [
      {"state": "California", "region": "West", "pop": 10000},
      {"state": "Texas", "region": "South", "pop": 5000},
      {"state": "Illinois", "region": "Midwest", "pop": 2000}
    ]
  }'
```

Expected response:
```json
{
  "success": true,
  "data": [
    {"state": "California", "pop": 10000}
  ]
}
```

### Test Error Handling

```bash
curl -X POST http://localhost:8080/execute \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT FROM table",
    "data": []
  }'
```

Expected response:
```json
{
  "success": false,
  "error": "no columns specified in SELECT"
}
```

## 3. Integration Testing with Frontend

1. **Start the Go server** (Terminal 1):
```bash
cd go
go run cmd/server/main.go
```

2. **Start the frontend** (Terminal 2):
```bash
npm run dev
```

3. **Open browser** to `http://localhost:5173`

4. **Load sample data**:
   - Click "Try sample JSON" → "US States Population"
   - Or paste your own JSON data

5. **Switch to Go parser**:
   - Select "Go (Server-side)" from the Parser dropdown
   - Verify the API URL is `http://localhost:8080`

6. **Test queries**:
   ```sql
   SELECT * FROM table
   SELECT state, pop FROM table WHERE pop > 5000
   SELECT state FROM table WHERE pop > 3000 AND region = 'West'
   SELECT state FROM table WHERE pop < 3000 OR (region = 'West' AND pop > 500)
   SELECT state FROM table WHERE pop_male > pop_female
   ```

7. **Compare results**:
   - Switch between "TypeScript" and "Go" parsers
   - Results should be identical
   - Check the query time difference

## 4. Performance Testing

### Compare Parser Performance

1. Load a large dataset (10k+ rows) in the frontend
2. Run the same query with both parsers
3. Compare execution times shown in the UI

### Benchmark Tests

Create a benchmark test:

```bash
cd go
go test -bench=. -benchmem ./...
```

## 5. Test Scripts

See `test-api.sh` for automated API testing.
