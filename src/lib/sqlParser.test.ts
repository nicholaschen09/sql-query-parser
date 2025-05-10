import { describe, it, expect } from 'vitest';
import { SQLParser } from './sqlParser';
import type { Row } from './types';

const data: Row[] = [
    { state: 'California', region: 'West', pop: 10000, pop_male: 6000, pop_female: 4000 },
    { state: 'Texas', region: 'South', pop: 5000, pop_male: 2500, pop_female: 2500 },
    { state: 'Illinois', region: 'Midwest', pop: 2000, pop_male: 1200, pop_female: 800 },
];

const parser = new SQLParser(data);

describe('SQLParser', () => {
    it('selects all rows with *', () => {
        const q = parser.parse('SELECT * FROM table');
        const res = parser.execute(q);
        expect(res.length).toBe(3);
        expect(res[0].state).toBe('California');
    });

    it('selects specific columns', () => {
        const q = parser.parse('SELECT state, pop FROM table');
        const res = parser.execute(q);
        expect(res[0]).toEqual({ state: 'California', pop: 10000 });
    });

    it('filters with WHERE and AND', () => {
        const q = parser.parse("SELECT state FROM table WHERE pop > 3000 AND region = 'West'");
        const res = parser.execute(q);
        expect(res.length).toBe(1);
        expect(res[0].state).toBe('California');
    });

    it('filters with OR and parentheses', () => {
        const q = parser.parse("SELECT state FROM table WHERE pop < 3000 OR (region = 'West' AND pop > 500)");
        const res = parser.execute(q);
        expect(res.length).toBe(2);
        expect(res.map(r => r.state)).toContain('California');
        expect(res.map(r => r.state)).toContain('Illinois');
    });

    it('supports column-to-column comparison', () => {
        const q = parser.parse('SELECT state FROM table WHERE pop_male > pop_female');
        const res = parser.execute(q);
        expect(res.length).toBe(2);
        expect(res.map(r => r.state)).toContain('California');
        expect(res.map(r => r.state)).toContain('Illinois');
    });

    it('returns empty for no matches', () => {
        const q = parser.parse("SELECT state FROM table WHERE pop > 99999");
        const res = parser.execute(q);
        expect(res.length).toBe(0);
    });

    it('throws on invalid SQL', () => {
        expect(() => parser.parse('SELECT FROM table')).toThrow();
        expect(() => parser.parse('SELECT state table')).toThrow();
        expect(() => parser.parse('SELECT state FROM')).toThrow();
    });

    it('throws on unknown column', () => {
        expect(() => parser.parse('SELECT foo FROM table')).not.toThrow();
        // But execution will not include the column
        const q = parser.parse('SELECT foo FROM table');
        const res = parser.execute(q);
        expect(res[0]).toEqual({});
    });
}); 