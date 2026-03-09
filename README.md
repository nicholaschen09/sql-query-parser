# SQL Query Parser (TypeScript)

<img width="990" height="1070" alt="Screenshot 2026-02-06 at 10 35 23 PM" src="https://github.com/user-attachments/assets/050132b3-5b34-40f6-aacb-6164b2743061" />

A TypeScript SQL query parser that runs in the browser and executes queries against JSON data.

## Features

- TypeScript-only parser and execution engine
- SvelteKit web interface for loading JSON and running SQL
- Query history in session storage
- Export query results as JSON, CSV, or Excel

## Project Structure

```text
sql-query-parser/
├── src/      # SvelteKit app + TypeScript parser
└── static/   # Static assets
```

## Prerequisites

- Node.js 18+
- npm 8+

## Installation

```bash
git clone https://github.com/nicholaschen09/sql-query-parser.git
cd sql-query-parser
npm install
```

## Running

```bash
npm run dev
```

Open `http://localhost:5173`.

## Testing

```bash
npx vitest run
```

## Example Queries

```sql
SELECT * FROM table;
SELECT state, pop FROM table WHERE pop > 1000000;
SELECT * FROM table WHERE pop > 1000000000 OR (pop > 1000000 AND region = 'Midwest');
SELECT state FROM table WHERE state != 'California' LIMIT 5;
```
