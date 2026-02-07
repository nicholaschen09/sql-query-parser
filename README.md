# SQL Query Parser

<img width="990" height="1070" alt="Screenshot 2026-02-06 at 10 35 23 PM" src="https://github.com/user-attachments/assets/050132b3-5b34-40f6-aacb-6164b2743061" />


A SQL query parser implementation in both TypeScript and Go that can query flat JSON objects. The application provides a web interface for executing SQL queries and viewing query history.

## Features

- **Dual Parser Support**: Choose between TypeScript (client-side) or Go (server-side) parser
- **Web Interface**: Interactive SvelteKit frontend for querying JSON data
- **Query History**: Track all your queries and results
- **Export Results**: Export query results as JSON, CSV, or Excel

## Prerequisites

- Node.js (v14 or higher)
- npm (v6 or higher)
- Go 1.21+ (optional, for Go parser)

## Installation

1. Clone the repository:
```bash
git clone https://github.com/nicholaschen09/sql-query-parser.git
cd sql-query-parser
```

2. Install dependencies:
```bash
npm install
```

## Running the Application

### Option 1: TypeScript Parser (Client-side)

1. Install dependencies:
```bash
npm install
```

2. Start the development server:
```bash
npm run dev
```

3. Open your browser and navigate to `http://localhost:5173`

The TypeScript parser runs entirely in the browser - no backend needed!

### Option 2: Go Parser (Server-side)

1. Start the Go server:
```bash
cd go
go mod tidy
go run cmd/server/main.go
```

The Go server will start on `http://localhost:8080` (or the port specified in the `PORT` environment variable).

2. In another terminal, start the frontend:
```bash
npm run dev
```

3. Open your browser and navigate to `http://localhost:5173`

4. In the UI, select "Go (Server-side)" from the Parser dropdown to use the Go parser.

### Switching Between Parsers

The frontend includes a parser selector that lets you switch between:
- **TypeScript (Client-side)**: Runs entirely in the browser, no backend required
- **Go (Server-side)**: Requires the Go server to be running, offers better performance for large datasets

See the [Go Parser README](go/README.md) for more details about the Go implementation.

## Example Queries

```sql
4
SELECT * FROM table WHERE pop > 1000000000 OR (pop > 1000000 AND region = 'Midwest');
SELECT * FROM table WHERE pop_male > pop_female;
```

## Limitations

- Only supports SELECT queries
- No support for GROUP BY, JOIN, or subqueries
- Only works with flat JSON objects (no nested objects or arrays)
- No support for NULL values
- Column names cannot be SQL reserved keywords
