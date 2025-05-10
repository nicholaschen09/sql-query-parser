import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import request from 'supertest';
import express from 'express';
import bodyParser from 'body-parser';
import { SQLParser } from '../lib/parser/sqlParser';
import type { Row } from '../lib/types/types';

// Sample data for testing
const data: Row[] = [
    { state: 'California', region: 'West', pop: 10000, pop_male: 6000, pop_female: 4000 },
    { state: 'Texas', region: 'South', pop: 5000, pop_male: 2500, pop_female: 2500 },
    { state: 'Illinois', region: 'Midwest', pop: 2000, pop_male: 1200, pop_female: 800 },
];

// Setup a minimal Express app for testing
const parser = new SQLParser(data);
const app = express();
app.use(bodyParser.json());
app.post('/api/query', (req, res) => {
    try {
        const { query } = req.body;
        const parsedQuery = parser.parse(query);
        const results = parser.execute(parsedQuery);
        res.json({ success: true, data: results });
    } catch (error) {
        res.json({ success: false, error: error instanceof Error ? error.message : 'Unknown error' });
    }
});

describe('Express API integration', () => {
    it('returns results for a valid query', async () => {
        const response = await request(app)
            .post('/api/query')
            .send({ query: "SELECT state FROM table WHERE pop > 3000" });
        expect(response.body.success).toBe(true);
        expect(response.body.data.length).toBe(2);
        expect(response.body.data[0].state).toBe('California');
    });

    it('returns error for invalid SQL', async () => {
        const response = await request(app)
            .post('/api/query')
            .send({ query: "SELECT FROM table" });
        expect(response.body.success).toBe(false);
        expect(response.body.error).toMatch(/No columns specified/i);
    });

    it('returns empty for no matches', async () => {
        const response = await request(app)
            .post('/api/query')
            .send({ query: "SELECT state FROM table WHERE pop > 99999" });
        expect(response.body.success).toBe(true);
        expect(response.body.data.length).toBe(0);
    });

    it('supports column-to-column comparison', async () => {
        const response = await request(app)
            .post('/api/query')
            .send({ query: "SELECT state FROM table WHERE pop_male > pop_female" });
        expect(response.body.success).toBe(true);
        expect(response.body.data.length).toBe(2);
        expect(response.body.data.map((r: any) => r.state)).toContain('California');
        expect(response.body.data.map((r: any) => r.state)).toContain('Illinois');
    });
}); 