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

CHALLENGE:

Given an array of flat JSON objects, write a program in Haskell to enable SQL querying it. You only need to support `SELECT (comma separated list of cols or \*) FROM TABLE WHERE (conditions) LIMIT (integer);`. No group by, join, subqueries etc.

Example Input:
```
[{ state: 'California', region: 'West', pop: 2312312321, pop_male: 3123123, pop_female: 123123 }, ...]
```

Examples:
```
SELECT state FROM table WHERE pop > 1000000 AND state != 'California';
SELECT * FROM table WHERE pop > 1000000000 OR (pop > 1000000 AND region = 'Midwest');
SELECT * FROM table WHERE pop_male > pop_female;
```

- Only need to support binary conditions `=`, `!=`, `<`, `>`, `AND`, `OR`, parentheses, and literals. Greater than/less than only need to work for numbers
- All JSON objects should have the same keys and only string/number vals, no nulls, arrays, nested objects, etc.
- Can fail inelegantly on runtime on empty array, unknown column/key, parse errors, type mismatch, column names with reserved keyword, etc. Not trying to trick you with edge cases, focus on the core flow.

Then, make the SQL parser into a web app. Let the user type in a query in a text box and get either the answer or the error on the other end. There should also be a list of all historical requests and their results below the text box/latest result.
- For the frontend, use SvelteKit. A single page is okay with everything done on the client side. Don't worry about CSS, as long as it's readable. Specifically, the SQL results should come back as a table if there isn't an error.
- For the backend, use Scotty as the web framework. You must use ScottyM (see `Web.Scotty.Trans`) as a monad transformer on top of `ReaderT Env`. `Env` is some configuration/environment type defined by you, probably should contain the DB connection and the parsed JSON input file.
- At the least, you probably need backend endpoints for handling query requests and getting historical query requests.
- Fail as gracefully as possible -- you can keep all the failure cases from the parser stage but the backend should not crash or fail silently to the user because of invalid SQL or failure to execute SQL on the input JSON.
- Use SQLite as the database and `sqlite-simple` as the haskell driver. You should only need one table that has the historical requests but not a hard restriction.
- Server should be started with one argument, the JSON file to query from. You can fail on launch if this fails to parse. The SQLite db can either be a fixed location, like ./db.db or as another argument.
- Submit as a github repo with instructions to build/run.

Recommend using aeson for JSON parsing and attoparsec (or megaparsec) for the SQL parsing. 
Criteria for quality:
- Correctness on valid queries
- Graceful failure on invalid queries -- do not crash or fail silently to the user given valid initial inputs
- General code structure and taste; if you don't know Haskell conventions that's fine, just do something that you find readable.
- No excessive optimizations, difficult to read function chaining, or typelevel programming.
