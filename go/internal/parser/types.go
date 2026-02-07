package parser

// Value represents a string or number value
type Value interface{}

// Row represents a row of data with string keys and Value values
type Row map[string]Value

// Operator represents a comparison or logical operator
type Operator string

const (
	OpEqual    Operator = "="
	OpNotEqual Operator = "!="
	OpLess     Operator = "<"
	OpGreater  Operator = ">"
	OpAnd      Operator = "AND"
	OpOr       Operator = "OR"
)

// Condition represents a WHERE condition
type Condition struct {
	Left     interface{} `json:"left"`     // string or Condition
	Operator Operator    `json:"operator"`
	Right    interface{} `json:"right"`    // string, number, or Condition
}

// SelectQuery represents a parsed SELECT query
type SelectQuery struct {
	Type    string    `json:"type"`
	Columns []string  `json:"columns"`
	Table   string    `json:"table"`
	Where   *Condition `json:"where,omitempty"`
	Limit   *int      `json:"limit,omitempty"`
}

// QueryResult represents the result of executing a query
type QueryResult struct {
	Success bool    `json:"success"`
	Data    []Row   `json:"data,omitempty"`
	Error   *string `json:"error,omitempty"`
}
