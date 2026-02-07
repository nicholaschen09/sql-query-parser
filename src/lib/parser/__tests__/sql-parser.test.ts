import { describe, it, expect } from 'vitest';
import { SQLParser } from '../sql-parser';
import type { Row } from '../../types/types';

// Existing test data (single table)
const data: Row[] = [
	{
		state: 'California',
		region: 'West',
		pop: 10000,
		pop_male: 6000,
		pop_female: 4000
	},
	{
		state: 'Texas',
		region: 'South',
		pop: 5000,
		pop_male: 2500,
		pop_female: 2500
	},
	{
		state: 'Illinois',
		region: 'Midwest',
		pop: 2000,
		pop_male: 1200,
		pop_female: 800
	}
];

// Multi-table test data
const users: Row[] = [
	{ id: 1, name: 'Alice', age: 30, address: { city: 'NYC', state: 'NY' } },
	{ id: 2, name: 'Bob', age: 25, address: { city: 'LA', state: 'CA' } },
	{ id: 3, name: 'Charlie', age: null, address: { city: 'Chicago', state: 'IL' } }
];

const orders: Row[] = [
	{ id: 101, user_id: 1, product: 'Widget', amount: 50 },
	{ id: 102, user_id: 1, product: 'Gadget', amount: 100 },
	{ id: 103, user_id: 2, product: 'Widget', amount: 30 }
];

const parser = new SQLParser(data);

describe('SQLParser - Existing Features', () => {
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
		const q = parser.parse(
			"SELECT state FROM table WHERE pop < 3000 OR (region = 'West' AND pop > 500)"
		);
		const res = parser.execute(q);
		expect(res.length).toBe(2);
		expect(res.map((r) => r.state)).toContain('California');
		expect(res.map((r) => r.state)).toContain('Illinois');
	});

	it('supports column-to-column comparison', () => {
		const q = parser.parse('SELECT state FROM table WHERE pop_male > pop_female');
		const res = parser.execute(q);
		expect(res.length).toBe(2);
		expect(res.map((r) => r.state)).toContain('California');
		expect(res.map((r) => r.state)).toContain('Illinois');
	});

	it('returns empty for no matches', () => {
		const q = parser.parse('SELECT state FROM table WHERE pop > 99999');
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
		const q = parser.parse('SELECT foo FROM table');
		const res = parser.execute(q);
		expect(res[0]).toEqual({});
	});

	it('supports LIMIT', () => {
		const q = parser.parse('SELECT * FROM table LIMIT 1');
		const res = parser.execute(q);
		expect(res.length).toBe(1);
	});
});

describe('SQLParser - NULL Support', () => {
	const p = new SQLParser({ users });

	it('supports IS NULL', () => {
		const q = p.parse('SELECT name FROM users WHERE age IS NULL');
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].name).toBe('Charlie');
	});

	it('supports IS NOT NULL', () => {
		const q = p.parse('SELECT name FROM users WHERE age IS NOT NULL');
		const res = p.execute(q);
		expect(res.length).toBe(2);
		expect(res.map((r) => r.name)).toContain('Alice');
		expect(res.map((r) => r.name)).toContain('Bob');
	});
});

describe('SQLParser - Nested Object Access', () => {
	const p = new SQLParser({ users });

	it('supports dot notation for nested objects in SELECT', () => {
		const q = p.parse('SELECT name, address.city FROM users');
		const res = p.execute(q);
		expect(res[0].name).toBe('Alice');
		expect(res[0]['address.city']).toBe('NYC');
	});

	it('supports dot notation in WHERE', () => {
		const q = p.parse("SELECT name FROM users WHERE address.city = 'LA'");
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].name).toBe('Bob');
	});
});

describe('SQLParser - JOIN Support', () => {
	const p = new SQLParser({ users, orders });

	it('supports INNER JOIN', () => {
		const q = p.parse(
			'SELECT users.name, orders.product FROM users JOIN orders ON users.id = orders.user_id'
		);
		const res = p.execute(q);
		expect(res.length).toBe(3); // Alice(2) + Bob(1)
		const names = res.map((r) => r['users.name']);
		expect(names.filter((n) => n === 'Alice').length).toBe(2);
		expect(names.filter((n) => n === 'Bob').length).toBe(1);
	});

	it('supports LEFT JOIN', () => {
		const q = p.parse(
			'SELECT users.name, orders.product FROM users LEFT JOIN orders ON users.id = orders.user_id'
		);
		const res = p.execute(q);
		expect(res.length).toBe(4); // Alice(2) + Bob(1) + Charlie(1 with null)
		const names = res.map((r) => r['users.name']);
		expect(names).toContain('Charlie');
		const charlieRow = res.find((r) => r['users.name'] === 'Charlie');
		expect(charlieRow?.['orders.product']).toBeNull();
	});

	it('supports RIGHT JOIN', () => {
		const p2 = new SQLParser({
			a: [
				{ id: 1, val: 'x' },
				{ id: 2, val: 'y' }
			],
			b: [
				{ id: 2, val: 'p' },
				{ id: 3, val: 'q' }
			]
		});
		const q = p2.parse('SELECT a.val, b.val FROM a RIGHT JOIN b ON a.id = b.id');
		const res = p2.execute(q);
		expect(res.length).toBe(2);
		// b.id=2 matches a.id=2, b.id=3 doesn't match
		const matched = res.find((r) => r['b.val'] === 'p');
		expect(matched?.['a.val']).toBe('y');
		const unmatched = res.find((r) => r['b.val'] === 'q');
		expect(unmatched?.['a.val']).toBeNull();
	});
});

describe('SQLParser - GROUP BY and Aggregations', () => {
	const p = new SQLParser({ orders });

	it('supports COUNT(*) with GROUP BY', () => {
		const q = p.parse('SELECT product, COUNT(*) AS cnt FROM orders GROUP BY product');
		const res = p.execute(q);
		expect(res.length).toBe(2);
		const widget = res.find((r) => r.product === 'Widget');
		expect(widget?.cnt).toBe(2);
		const gadget = res.find((r) => r.product === 'Gadget');
		expect(gadget?.cnt).toBe(1);
	});

	it('supports SUM with GROUP BY', () => {
		const q = p.parse('SELECT user_id, SUM(amount) AS total FROM orders GROUP BY user_id');
		const res = p.execute(q);
		const alice = res.find((r) => r.user_id === 1);
		expect(alice?.total).toBe(150);
		const bob = res.find((r) => r.user_id === 2);
		expect(bob?.total).toBe(30);
	});

	it('supports AVG without GROUP BY', () => {
		const q = p.parse('SELECT AVG(amount) AS avg_amount FROM orders');
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].avg_amount).toBe(60); // (50+100+30)/3
	});

	it('supports MIN and MAX', () => {
		const q = p.parse('SELECT MIN(amount) AS min_amt, MAX(amount) AS max_amt FROM orders');
		const res = p.execute(q);
		expect(res[0].min_amt).toBe(30);
		expect(res[0].max_amt).toBe(100);
	});

	it('supports HAVING', () => {
		const q = p.parse(
			'SELECT user_id, COUNT(*) AS cnt FROM orders GROUP BY user_id HAVING cnt > 1'
		);
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].user_id).toBe(1);
	});
});

describe('SQLParser - Subquery Support', () => {
	const p = new SQLParser({ users, orders });

	it('supports IN with subquery', () => {
		const q = p.parse(
			'SELECT name FROM users WHERE id IN (SELECT user_id FROM orders WHERE amount > 25)'
		);
		const res = p.execute(q);
		expect(res.length).toBe(2);
		expect(res.map((r) => r.name)).toContain('Alice');
		expect(res.map((r) => r.name)).toContain('Bob');
	});

	it('supports NOT IN with subquery', () => {
		const q = p.parse(
			'SELECT name FROM users WHERE id NOT IN (SELECT user_id FROM orders)'
		);
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].name).toBe('Charlie');
	});
});

describe('SQLParser - LIKE Support', () => {
	const p = new SQLParser({ users });

	it('supports LIKE with % wildcard', () => {
		const q = p.parse("SELECT name FROM users WHERE name LIKE 'A%'");
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].name).toBe('Alice');
	});

	it('supports LIKE with _ wildcard', () => {
		const q = p.parse("SELECT name FROM users WHERE name LIKE 'Bo_'");
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].name).toBe('Bob');
	});

	it('supports NOT LIKE', () => {
		const q = p.parse("SELECT name FROM users WHERE name NOT LIKE 'A%'");
		const res = p.execute(q);
		expect(res.length).toBe(2);
	});
});

describe('SQLParser - IN with Value List', () => {
	const p = new SQLParser({ users });

	it('supports IN with numbers', () => {
		const q = p.parse('SELECT name FROM users WHERE age IN (25, 30)');
		const res = p.execute(q);
		expect(res.length).toBe(2);
		expect(res.map((r) => r.name)).toContain('Alice');
		expect(res.map((r) => r.name)).toContain('Bob');
	});

	it('supports NOT IN with numbers', () => {
		const q = p.parse('SELECT name FROM users WHERE age NOT IN (30)');
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].name).toBe('Bob');
	});

	it('supports IN with strings', () => {
		const q = p.parse("SELECT name FROM users WHERE name IN ('Alice', 'Charlie')");
		const res = p.execute(q);
		expect(res.length).toBe(2);
		expect(res.map((r) => r.name)).toContain('Alice');
		expect(res.map((r) => r.name)).toContain('Charlie');
	});
});

describe('SQLParser - ORDER BY', () => {
	const p = new SQLParser(data);

	it('supports ORDER BY ASC (default)', () => {
		const q = p.parse('SELECT state, pop FROM table ORDER BY pop');
		const res = p.execute(q);
		expect(res[0].state).toBe('Illinois');
		expect(res[2].state).toBe('California');
	});

	it('supports ORDER BY DESC', () => {
		const q = p.parse('SELECT state, pop FROM table ORDER BY pop DESC');
		const res = p.execute(q);
		expect(res[0].state).toBe('California');
		expect(res[2].state).toBe('Illinois');
	});

	it('supports ORDER BY with multiple columns', () => {
		const p2 = new SQLParser([
			{ name: 'A', score: 10 },
			{ name: 'B', score: 20 },
			{ name: 'C', score: 10 }
		]);
		const q = p2.parse('SELECT * FROM table ORDER BY score ASC, name DESC');
		const res = p2.execute(q);
		expect(res[0].name).toBe('C');
		expect(res[1].name).toBe('A');
		expect(res[2].name).toBe('B');
	});
});

describe('SQLParser - Comparison Operators', () => {
	const p = new SQLParser(data);

	it('supports >=', () => {
		const q = p.parse('SELECT state FROM table WHERE pop >= 5000');
		const res = p.execute(q);
		expect(res.length).toBe(2);
		expect(res.map((r) => r.state)).toContain('California');
		expect(res.map((r) => r.state)).toContain('Texas');
	});

	it('supports <=', () => {
		const q = p.parse('SELECT state FROM table WHERE pop <= 5000');
		const res = p.execute(q);
		expect(res.length).toBe(2);
		expect(res.map((r) => r.state)).toContain('Texas');
		expect(res.map((r) => r.state)).toContain('Illinois');
	});
});

describe('SQLParser - Multi-word String Literals', () => {
	const p = new SQLParser([
		{ city: 'New York', pop: 8000000 },
		{ city: 'Los Angeles', pop: 4000000 },
		{ city: 'Chicago', pop: 2700000 }
	]);

	it('handles string literals with spaces', () => {
		const q = p.parse("SELECT * FROM table WHERE city = 'New York'");
		const res = p.execute(q);
		expect(res.length).toBe(1);
		expect(res[0].city).toBe('New York');
	});
});
