package parser

import (
	"testing"
)

var testData = []Row{
	{"state": "California", "region": "West", "pop": 10000, "pop_male": 6000, "pop_female": 4000},
	{"state": "Texas", "region": "South", "pop": 5000, "pop_male": 2500, "pop_female": 2500},
	{"state": "Illinois", "region": "Midwest", "pop": 2000, "pop_male": 1200, "pop_female": 800},
}

func singleTableParser() *SQLParser {
	return NewSQLParserFromData(testData)
}

func TestSQLParser(t *testing.T) {
	parser := singleTableParser()

	t.Run("selects all rows with *", func(t *testing.T) {
		q, err := parser.Parse("SELECT * FROM table")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := parser.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 3 {
			t.Errorf("Expected 3 rows, got %d", len(res))
		}
		if res[0]["state"] != "California" {
			t.Errorf("Expected California, got %v", res[0]["state"])
		}
	})

	t.Run("selects specific columns", func(t *testing.T) {
		q, err := parser.Parse("SELECT state, pop FROM table")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := parser.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 3 {
			t.Errorf("Expected 3 rows, got %d", len(res))
		}
		if res[0]["state"] != "California" || res[0]["pop"] != 10000 {
			t.Errorf("Unexpected result: %v", res[0])
		}
		if _, ok := res[0]["region"]; ok {
			t.Errorf("Expected region to be excluded")
		}
	})

	t.Run("filters with WHERE and AND", func(t *testing.T) {
		q, err := parser.Parse("SELECT state FROM table WHERE pop > 3000 AND region = 'West'")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := parser.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 1 {
			t.Errorf("Expected 1 row, got %d", len(res))
		}
		if res[0]["state"] != "California" {
			t.Errorf("Expected California, got %v", res[0]["state"])
		}
	})

	t.Run("filters with OR and parentheses", func(t *testing.T) {
		q, err := parser.Parse("SELECT state FROM table WHERE pop < 3000 OR (region = 'West' AND pop > 500)")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := parser.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 rows, got %d", len(res))
		}
	})

	t.Run("supports column-to-column comparison", func(t *testing.T) {
		q, err := parser.Parse("SELECT state FROM table WHERE pop_male > pop_female")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := parser.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 rows, got %d", len(res))
		}
	})

	t.Run("returns empty for no matches", func(t *testing.T) {
		q, err := parser.Parse("SELECT state FROM table WHERE pop > 99999")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := parser.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 0 {
			t.Errorf("Expected 0 rows, got %d", len(res))
		}
	})

	t.Run("throws on invalid SQL", func(t *testing.T) {
		_, err := parser.Parse("SELECT FROM table")
		if err == nil {
			t.Error("Expected error for invalid SQL")
		}
		_, err = parser.Parse("SELECT state table")
		if err == nil {
			t.Error("Expected error for missing FROM")
		}
		_, err = parser.Parse("SELECT state FROM")
		if err == nil {
			t.Error("Expected error for missing table name")
		}
	})
}

func TestNullSupport(t *testing.T) {
	users := []Row{
		{"id": 1, "name": "Alice", "age": 30},
		{"id": 2, "name": "Bob", "age": 25},
		{"id": 3, "name": "Charlie", "age": nil},
	}
	p := NewSQLParser(map[string][]Row{"users": users})

	t.Run("IS NULL", func(t *testing.T) {
		q, err := p.Parse("SELECT name FROM users WHERE age IS NULL")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 1 || res[0]["name"] != "Charlie" {
			t.Errorf("Expected Charlie, got %v", res)
		}
	})

	t.Run("IS NOT NULL", func(t *testing.T) {
		q, err := p.Parse("SELECT name FROM users WHERE age IS NOT NULL")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 rows, got %d", len(res))
		}
	})
}

func TestGroupByAndAggregations(t *testing.T) {
	orders := []Row{
		{"id": 101, "user_id": 1, "product": "Widget", "amount": 50},
		{"id": 102, "user_id": 1, "product": "Gadget", "amount": 100},
		{"id": 103, "user_id": 2, "product": "Widget", "amount": 30},
	}
	p := NewSQLParser(map[string][]Row{"orders": orders})

	t.Run("COUNT with GROUP BY", func(t *testing.T) {
		q, err := p.Parse("SELECT product, COUNT(*) AS cnt FROM orders GROUP BY product")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 groups, got %d", len(res))
		}
	})

	t.Run("SUM with GROUP BY", func(t *testing.T) {
		q, err := p.Parse("SELECT user_id, SUM(amount) AS total FROM orders GROUP BY user_id")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		for _, row := range res {
			if compareValues(row["user_id"], 1) == 0 {
				if compareValues(row["total"], 150) != 0 {
					t.Errorf("Expected total 150 for user 1, got %v", row["total"])
				}
			}
		}
	})

	t.Run("AVG without GROUP BY", func(t *testing.T) {
		q, err := p.Parse("SELECT AVG(amount) AS avg_amount FROM orders")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 1 {
			t.Errorf("Expected 1 row, got %d", len(res))
		}
		if compareValues(res[0]["avg_amount"], 60) != 0 {
			t.Errorf("Expected avg 60, got %v", res[0]["avg_amount"])
		}
	})

	t.Run("HAVING", func(t *testing.T) {
		q, err := p.Parse("SELECT user_id, COUNT(*) AS cnt FROM orders GROUP BY user_id HAVING cnt > 1")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 1 {
			t.Errorf("Expected 1 group, got %d", len(res))
		}
	})
}

func TestOrderBy(t *testing.T) {
	p := singleTableParser()

	t.Run("ORDER BY ASC", func(t *testing.T) {
		q, err := p.Parse("SELECT state, pop FROM table ORDER BY pop")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if res[0]["state"] != "Illinois" {
			t.Errorf("Expected Illinois first, got %v", res[0]["state"])
		}
	})

	t.Run("ORDER BY DESC", func(t *testing.T) {
		q, err := p.Parse("SELECT state, pop FROM table ORDER BY pop DESC")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if res[0]["state"] != "California" {
			t.Errorf("Expected California first, got %v", res[0]["state"])
		}
	})
}

func TestComparisonOperators(t *testing.T) {
	p := singleTableParser()

	t.Run(">=", func(t *testing.T) {
		q, err := p.Parse("SELECT state FROM table WHERE pop >= 5000")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 rows, got %d", len(res))
		}
	})

	t.Run("<=", func(t *testing.T) {
		q, err := p.Parse("SELECT state FROM table WHERE pop <= 5000")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 rows, got %d", len(res))
		}
	})
}

func TestLike(t *testing.T) {
	users := []Row{
		{"name": "Alice"}, {"name": "Bob"}, {"name": "Charlie"},
	}
	p := NewSQLParser(map[string][]Row{"users": users})

	t.Run("LIKE with %", func(t *testing.T) {
		q, err := p.Parse("SELECT name FROM users WHERE name LIKE 'A%'")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 1 || res[0]["name"] != "Alice" {
			t.Errorf("Expected Alice, got %v", res)
		}
	})

	t.Run("NOT LIKE", func(t *testing.T) {
		q, err := p.Parse("SELECT name FROM users WHERE name NOT LIKE 'A%'")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 rows, got %d", len(res))
		}
	})
}

func TestInValueList(t *testing.T) {
	users := []Row{
		{"id": 1, "name": "Alice", "age": 30},
		{"id": 2, "name": "Bob", "age": 25},
		{"id": 3, "name": "Charlie", "age": nil},
	}
	p := NewSQLParser(map[string][]Row{"users": users})

	t.Run("IN with numbers", func(t *testing.T) {
		q, err := p.Parse("SELECT name FROM users WHERE age IN (25, 30)")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 rows, got %d", len(res))
		}
	})

	t.Run("NOT IN", func(t *testing.T) {
		q, err := p.Parse("SELECT name FROM users WHERE age NOT IN (30)")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 1 || res[0]["name"] != "Bob" {
			t.Errorf("Expected Bob, got %v", res)
		}
	})
}

func TestSubquery(t *testing.T) {
	users := []Row{
		{"id": 1, "name": "Alice"},
		{"id": 2, "name": "Bob"},
		{"id": 3, "name": "Charlie"},
	}
	orders := []Row{
		{"user_id": 1, "amount": 50},
		{"user_id": 1, "amount": 100},
		{"user_id": 2, "amount": 30},
	}
	p := NewSQLParser(map[string][]Row{"users": users, "orders": orders})

	t.Run("IN with subquery", func(t *testing.T) {
		q, err := p.Parse("SELECT name FROM users WHERE id IN (SELECT user_id FROM orders WHERE amount > 25)")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 2 {
			t.Errorf("Expected 2 rows, got %d", len(res))
		}
	})
}

func TestJoin(t *testing.T) {
	users := []Row{
		{"id": 1, "name": "Alice"},
		{"id": 2, "name": "Bob"},
		{"id": 3, "name": "Charlie"},
	}
	orders := []Row{
		{"id": 101, "user_id": 1, "product": "Widget"},
		{"id": 102, "user_id": 1, "product": "Gadget"},
		{"id": 103, "user_id": 2, "product": "Widget"},
	}
	p := NewSQLParser(map[string][]Row{"users": users, "orders": orders})

	t.Run("INNER JOIN", func(t *testing.T) {
		q, err := p.Parse("SELECT users.name, orders.product FROM users JOIN orders ON users.id = orders.user_id")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 3 {
			t.Errorf("Expected 3 rows, got %d", len(res))
		}
	})

	t.Run("LEFT JOIN", func(t *testing.T) {
		q, err := p.Parse("SELECT users.name, orders.product FROM users LEFT JOIN orders ON users.id = orders.user_id")
		if err != nil {
			t.Fatalf("Parse error: %v", err)
		}
		res, err := p.Execute(q)
		if err != nil {
			t.Fatalf("Execute error: %v", err)
		}
		if len(res) != 4 {
			t.Errorf("Expected 4 rows (includes Charlie with NULL), got %d", len(res))
		}
	})
}
