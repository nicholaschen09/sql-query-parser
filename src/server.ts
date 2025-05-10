import express from 'express';
import cors from 'cors';
import Database from 'better-sqlite3';
import { readFileSync } from 'fs';
import { SQLParser } from './lib/parser/sqlParser.js';
import type { QueryResult, QueryHistory } from './lib/types/types.js';

const app = express();
const port = process.env.PORT || 3000;

// Initialize SQLite database
const db = new Database('queries.db');
db.exec(`
    CREATE TABLE IF NOT EXISTS query_history (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        query TEXT NOT NULL,
        result TEXT NOT NULL,
        timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
    )
`);

// Load and parse the JSON data
const jsonData = JSON.parse(readFileSync(process.argv[2], 'utf-8'));
const parser = new SQLParser(jsonData);

app.use(cors());
app.use(express.json());

// Endpoint to execute SQL queries
app.post('/api/query', (req: express.Request, res: express.Response) => {
    try {
        const { query } = req.body;
        const parsedQuery = parser.parse(query);
        const results = parser.execute(parsedQuery);

        const queryResult: QueryResult = {
            success: true,
            data: results
        };

        // Store in history
        const stmt = db.prepare('INSERT INTO query_history (query, result) VALUES (?, ?)');
        stmt.run(query, JSON.stringify(queryResult));

        res.json(queryResult);
    } catch (error) {
        const errorResult: QueryResult = {
            success: false,
            error: error instanceof Error ? error.message : 'Unknown error occurred'
        };

        // Store error in history
        const stmt = db.prepare('INSERT INTO query_history (query, result) VALUES (?, ?)');
        stmt.run(req.body.query, JSON.stringify(errorResult));

        res.json(errorResult);
    }
});

// Endpoint to get query history
app.get('/api/history', (req: express.Request, res: express.Response) => {
    const stmt = db.prepare('SELECT * FROM query_history ORDER BY timestamp DESC');
    const history = stmt.all().map((row: any) => ({
        id: row.id,
        query: row.query,
        result: JSON.parse(row.result),
        timestamp: row.timestamp
    }));
    res.json(history);
});

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
}); 