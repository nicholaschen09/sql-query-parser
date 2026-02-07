# SQL Query Parser

<img width="990" height="1070" alt="Screenshot 2026-02-06 at 10 35 23 PM" src="https://github.com/user-attachments/assets/050132b3-5b34-40f6-aacb-6164b2743061" />

A SQL query parser built in four languages — TypeScript, Go, Rust, and Haskell — that can query flat JSON objects. The application provides a minimal web interface for executing SQL queries, viewing results as tables, and browsing query history.

## Features

- **Multi-Parser Support**: Choose between TypeScript (client-side), Go, Rust, or Haskell (server-side) parsers
- **Web Interface**: Interactive SvelteKit frontend for querying JSON data
- **Query History**: Track all your queries and results
- **Export Results**: Export query results as JSON, CSV, or Excel
- **Blog**: Built-in blog page explaining how the parser works

## Project Structure

```
sql-query-parser/
├── src/                  # SvelteKit frontend (TypeScript parser included)
├── go/                   # Go parser + HTTP server (port 8080)
├── rust/                 # Rust parser + HTTP server (port 8081)
├── haskell/              # Haskell parser + HTTP server (port 8082)
└── static/               # Static assets
```

## Prerequisites

- Node.js (v18 or higher)
- npm (v8 or higher)
- Go 1.21+ (optional, for Go parser)
- Rust / Cargo (optional, for Rust parser)
- GHC / Cabal (optional, for Haskell parser)

## Installation

1. Clone the repository:
```bash
git clone https://github.com/nicholaschen09/sql-query-parser.git
cd sql-query-parser
```

2. Install frontend dependencies:
```bash
npm install
```

## Running the Application

### Frontend

```bash
npm run dev
```

Open your browser at `http://localhost:5173`. The TypeScript parser runs entirely in the browser — no backend needed.

### Go Parser (Server-side)

```bash
cd go
go mod tidy
go run cmd/server/main.go
```

Runs on `http://localhost:8080`. Select **Go (Server-side)** from the parser dropdown in the UI.

### Rust Parser (Server-side)

```bash
cd rust
cargo run
```

Runs on `http://localhost:8081`. Select **Rust (Server-side)** from the parser dropdown in the UI.

### Haskell Parser (Server-side)

```bash
cd haskell
cabal build
cabal run sql-query-parser
```

Runs on `http://localhost:8082`. Select **Haskell (Server-side)** from the parser dropdown in the UI.

## Switching Between Parsers

The frontend includes a parser selector dropdown that lets you switch between:

- **TypeScript (Client-side)** — Runs entirely in the browser, no backend required
- **Go (Server-side)** — Requires the Go server on port 8080
- **Rust (Server-side)** — Requires the Rust server on port 8081
- **Haskell (Server-side)** — Requires the Haskell server on port 8082

## Running Tests

```bash
# TypeScript
npm test

# Go
cd go && go test ./...

# Rust
cd rust && cargo test

# Haskell
cd haskell && cabal test
```

## Example Queries

```sql
SELECT * FROM table;
SELECT state, pop FROM table WHERE pop > 1000000;
SELECT * FROM table WHERE pop > 1000000000 OR (pop > 1000000 AND region = 'Midwest');
SELECT * FROM table WHERE pop_male > pop_female;
SELECT state FROM table WHERE state != 'California' LIMIT 5;
```

## Limitations

- Only supports SELECT queries
- No support for GROUP BY, JOIN, or subqueries
- Only works with flat JSON objects (no nested objects or arrays)
- No support for NULL values
- Column names cannot be SQL reserved keywords
