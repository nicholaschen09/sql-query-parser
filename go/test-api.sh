#!/bin/bash

# Test script for Go SQL Parser API
# Make sure the server is running: go run cmd/server/main.go

API_URL="${API_URL:-http://localhost:8080}"

echo "Testing Go SQL Parser API at $API_URL"
echo "======================================"
echo ""

# Test 1: Health check
echo "1. Testing health endpoint..."
curl -s "$API_URL/health" | jq .
echo ""
echo ""

# Test 2: Simple SELECT *
echo "2. Testing SELECT * FROM table..."
curl -s -X POST "$API_URL/execute" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT * FROM table",
    "data": [
      {"state": "California", "region": "West", "pop": 10000},
      {"state": "Texas", "region": "South", "pop": 5000}
    ]
  }' | jq .
echo ""
echo ""

# Test 3: SELECT specific columns
echo "3. Testing SELECT state, pop FROM table..."
curl -s -X POST "$API_URL/execute" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT state, pop FROM table",
    "data": [
      {"state": "California", "region": "West", "pop": 10000},
      {"state": "Texas", "region": "South", "pop": 5000}
    ]
  }' | jq .
echo ""
echo ""

# Test 4: WHERE clause with AND
echo "4. Testing WHERE pop > 5000 AND region = '\''West'\''..."
curl -s -X POST "$API_URL/execute" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT state FROM table WHERE pop > 5000 AND region = '\''West'\''",
    "data": [
      {"state": "California", "region": "West", "pop": 10000},
      {"state": "Texas", "region": "South", "pop": 5000},
      {"state": "Illinois", "region": "Midwest", "pop": 2000}
    ]
  }' | jq .
echo ""
echo ""

# Test 5: WHERE clause with OR and parentheses
echo "5. Testing WHERE with OR and parentheses..."
curl -s -X POST "$API_URL/execute" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT state FROM table WHERE pop < 3000 OR (region = '\''West'\'' AND pop > 500)",
    "data": [
      {"state": "California", "region": "West", "pop": 10000},
      {"state": "Texas", "region": "South", "pop": 5000},
      {"state": "Illinois", "region": "Midwest", "pop": 2000}
    ]
  }' | jq .
echo ""
echo ""

# Test 6: Column-to-column comparison
echo "6. Testing column-to-column comparison..."
curl -s -X POST "$API_URL/execute" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT state FROM table WHERE pop_male > pop_female",
    "data": [
      {"state": "California", "pop_male": 6000, "pop_female": 4000},
      {"state": "Texas", "pop_male": 2500, "pop_female": 2500},
      {"state": "Illinois", "pop_male": 1200, "pop_female": 800}
    ]
  }' | jq .
echo ""
echo ""

# Test 7: LIMIT clause
echo "7. Testing LIMIT clause..."
curl -s -X POST "$API_URL/execute" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT state FROM table LIMIT 2",
    "data": [
      {"state": "California", "region": "West", "pop": 10000},
      {"state": "Texas", "region": "South", "pop": 5000},
      {"state": "Illinois", "region": "Midwest", "pop": 2000}
    ]
  }' | jq .
echo ""
echo ""

# Test 8: Error handling - invalid SQL
echo "8. Testing error handling (invalid SQL)..."
curl -s -X POST "$API_URL/execute" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT FROM table",
    "data": []
  }' | jq .
echo ""
echo ""

# Test 9: Empty result
echo "9. Testing empty result..."
curl -s -X POST "$API_URL/execute" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT state FROM table WHERE pop > 99999",
    "data": [
      {"state": "California", "region": "West", "pop": 10000}
    ]
  }' | jq .
echo ""
echo ""

echo "======================================"
echo "All tests completed!"
