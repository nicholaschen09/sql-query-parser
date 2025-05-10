# SQL Query Parser

A TypeScript implementation of a SQL query parser that can query flat JSON objects. The application provides a web interface for executing SQL queries and viewing query history.

## Features

- Parse and execute SQL SELECT queries on JSON data
- Support for WHERE clauses with conditions (=, !=, <, >, AND, OR)
- Support for LIMIT clause
- Query history tracking
- Web interface built with SvelteKit
- SQLite database for storing query history

## Prerequisites

- Node.js (v14 or higher)
- npm (v6 or higher)

## Installation

1. Clone the repository:
```bash
git clone <repository-url>
cd sql-query-parser
```

2. Install dependencies:
```bash
npm install
```

## Running the Application

1. Start the backend server:
```bash
npm run server -- path/to/your/data.json
```

2. In a separate terminal, start the frontend development server:
```bash
npm run dev
```

3. Open your browser and navigate to `http://localhost:5173`

## Example Queries

```sql
SELECT state FROM table WHERE pop > 1000000 AND state != 'California';
SELECT * FROM table WHERE pop > 1000000000 OR (pop > 1000000 AND region = 'Midwest');
SELECT * FROM table WHERE pop_male > pop_female;
```

## Project Structure

- `src/lib/types.ts` - TypeScript type definitions
- `src/lib/sqlParser.ts` - SQL parser implementation
- `src/server.ts` - Express backend server
- `src/routes/+page.svelte` - SvelteKit frontend page

## Limitations

- Only supports SELECT queries
- No support for GROUP BY, JOIN, or subqueries
- Only works with flat JSON objects (no nested objects or arrays)
- No support for NULL values
- Column names cannot be SQL reserved keywords
