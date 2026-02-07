package parser

import (
	"testing"
)

func TestSQLParser(t *testing.T) {
	data := []Row{
		{"state": "California", "region": "West", "pop": 10000, "pop_male": 6000, "pop_female": 4000},
		{"state": "Texas", "region": "South", "pop": 5000, "pop_male": 2500, "pop_female": 2500},
		{"state": "Illinois", "region": "Midwest", "pop": 2000, "pop_male": 1200, "pop_female": 800},
	}

	parser := NewSQLParser(data)

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
		states := []string{}
		for _, row := range res {
			states = append(states, row["state"].(string))
		}
		if !contains(states, "California") || !contains(states, "Illinois") {
			t.Errorf("Expected California and Illinois, got %v", states)
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
		states := []string{}
		for _, row := range res {
			states = append(states, row["state"].(string))
		}
		if !contains(states, "California") || !contains(states, "Illinois") {
			t.Errorf("Expected California and Illinois, got %v", states)
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

func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}
