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
	OpLessEq   Operator = "<="
	OpGreaterEq Operator = ">="
	OpAnd      Operator = "AND"
	OpOr       Operator = "OR"
	OpLike     Operator = "LIKE"
	OpNotLike  Operator = "NOT LIKE"
	OpIn       Operator = "IN"
	OpNotIn    Operator = "NOT IN"
	OpIs       Operator = "IS"
	OpIsNot    Operator = "IS NOT"
)

// Condition represents a WHERE condition
type Condition struct {
	Left     interface{} `json:"left"`     // string or *Condition
	Operator Operator    `json:"operator"`
	Right    interface{} `json:"right"`    // string, number, *Condition, []interface{}, *SelectQuery, nil
}

// AggregateColumn represents a column with an aggregate function
type AggregateColumn struct {
	Func   string `json:"func"`
	Column string `json:"column"`
	Alias  string `json:"alias,omitempty"`
}

// ColumnExpr represents a column expression (plain string or aggregate)
type ColumnExpr struct {
	IsAggregate bool
	Name        string // column name or "*"
	Func        string // aggregate function name (COUNT, SUM, AVG, MIN, MAX)
	Alias       string // alias from AS
}

// JoinClause represents a JOIN clause
type JoinClause struct {
	JoinType string     `json:"joinType"` // INNER, LEFT, RIGHT
	Table    string     `json:"table"`
	On       *Condition `json:"on"`
}

// OrderByClause represents an ORDER BY clause
type OrderByClause struct {
	Column    string `json:"column"`
	Direction string `json:"direction"` // ASC or DESC
}

// SelectQuery represents a parsed SELECT query
type SelectQuery struct {
	Type    string          `json:"type"`
	Columns []ColumnExpr    `json:"columns"`
	Table   string          `json:"table"`
	Joins   []JoinClause    `json:"joins,omitempty"`
	Where   *Condition      `json:"where,omitempty"`
	GroupBy []string        `json:"groupBy,omitempty"`
	Having  *Condition      `json:"having,omitempty"`
	OrderBy []OrderByClause `json:"orderBy,omitempty"`
	Limit   *int            `json:"limit,omitempty"`
}

// QueryResult represents the result of executing a query
type QueryResult struct {
	Success bool    `json:"success"`
	Data    []Row   `json:"data,omitempty"`
	Error   *string `json:"error,omitempty"`
}
