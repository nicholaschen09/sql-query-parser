package parser

import (
	"fmt"
	"strconv"
	"strings"
)

// SQLParser parses and executes SQL queries
type SQLParser struct {
	data []Row
}

// NewSQLParser creates a new SQL parser with the given data
func NewSQLParser(data []Row) *SQLParser {
	return &SQLParser{data: data}
}

// Parse parses a SQL query string into a SelectQuery
func (p *SQLParser) Parse(query string) (*SelectQuery, error) {
	tokens := p.tokenize(query)
	return p.parseSelect(tokens)
}

// Execute executes a parsed query against the data
func (p *SQLParser) Execute(query *SelectQuery) ([]Row, error) {
	results := make([]Row, len(p.data))
	copy(results, p.data)

	// Apply WHERE clause
	if query.Where != nil {
		filtered := []Row{}
		for _, row := range results {
			if p.evaluateCondition(row, query.Where) {
				filtered = append(filtered, row)
			}
		}
		results = filtered
	}

	// Apply column selection
	if len(query.Columns) > 0 && query.Columns[0] != "*" {
		for i, row := range results {
			newRow := make(Row)
			for _, col := range query.Columns {
				if val, ok := row[col]; ok {
					newRow[col] = val
				}
			}
			results[i] = newRow
		}
	}

	// Apply LIMIT
	if query.Limit != nil {
		limit := *query.Limit
		if limit < len(results) {
			results = results[:limit]
		}
	}

	return results, nil
}

func (p *SQLParser) tokenize(query string) []string {
	// Replace special characters with spaces
	query = strings.ReplaceAll(query, "(", " ( ")
	query = strings.ReplaceAll(query, ")", " ) ")
	query = strings.ReplaceAll(query, ",", " , ")
	query = strings.TrimSuffix(query, ";")

	// Split by whitespace
	parts := strings.Fields(query)
	tokens := []string{}
	for _, part := range parts {
		if len(part) > 0 {
			tokens = append(tokens, part)
		}
	}
	return tokens
}

func (p *SQLParser) parseSelect(tokens []string) (*SelectQuery, error) {
	if len(tokens) == 0 || strings.ToUpper(tokens[0]) != "SELECT" {
		return nil, fmt.Errorf("query must start with SELECT")
	}

	// Find FROM index
	fromIndex := -1
	for i, token := range tokens {
		if strings.ToUpper(token) == "FROM" {
			fromIndex = i
			break
		}
	}
	if fromIndex == -1 {
		return nil, fmt.Errorf("expected FROM clause")
	}

	// Parse columns
	if fromIndex <= 1 {
		return nil, fmt.Errorf("no columns specified in SELECT")
	}
	columns := p.parseColumns(tokens[1:fromIndex])
	if len(columns) == 0 || (len(columns) == 1 && columns[0] == "") {
		return nil, fmt.Errorf("no columns specified in SELECT")
	}

	// Parse table name
	if fromIndex+1 >= len(tokens) {
		return nil, fmt.Errorf("expected table name after FROM")
	}
	table := tokens[fromIndex+1]

	query := &SelectQuery{
		Type:    "SELECT",
		Columns: columns,
		Table:   table,
	}

	currentIndex := fromIndex + 2

	// Parse WHERE clause
	if currentIndex < len(tokens) && strings.ToUpper(tokens[currentIndex]) == "WHERE" {
		currentIndex++
		cond, nextIdx, err := p.parseConditionRecursive(tokens, currentIndex)
		if err != nil {
			return nil, err
		}
		query.Where = cond
		currentIndex = nextIdx
	}

	// Parse LIMIT clause
	if currentIndex < len(tokens) && strings.ToUpper(tokens[currentIndex]) == "LIMIT" {
		currentIndex++
		if currentIndex >= len(tokens) {
			return nil, fmt.Errorf("expected number after LIMIT")
		}
		limit, err := strconv.Atoi(tokens[currentIndex])
		if err != nil || limit < 0 {
			return nil, fmt.Errorf("LIMIT must be a non-negative integer")
		}
		query.Limit = &limit
	}

	return query, nil
}

func (p *SQLParser) parseColumns(tokens []string) []string {
	columns := []string{}
	for _, token := range tokens {
		if token != "," && token != "" {
			col := strings.Trim(token, ",")
			if col != "" {
				columns = append(columns, col)
			}
		}
	}
	return columns
}

// Parse OR (lowest precedence)
func (p *SQLParser) parseConditionRecursive(tokens []string, idx int) (*Condition, int, error) {
	left, nextIdx, err := p.parseAnd(tokens, idx)
	if err != nil {
		return nil, 0, err
	}

	for nextIdx < len(tokens) && strings.ToUpper(tokens[nextIdx]) == "OR" {
		operator := Operator(strings.ToUpper(tokens[nextIdx]))
		right, afterRight, err := p.parseAnd(tokens, nextIdx+1)
		if err != nil {
			return nil, 0, err
		}
		left = &Condition{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
		nextIdx = afterRight
	}

	return left, nextIdx, nil
}

// Parse AND (higher precedence)
func (p *SQLParser) parseAnd(tokens []string, idx int) (*Condition, int, error) {
	left, nextIdx, err := p.parseOperand(tokens, idx)
	if err != nil {
		return nil, 0, err
	}

	for nextIdx < len(tokens) && strings.ToUpper(tokens[nextIdx]) == "AND" {
		operator := Operator(strings.ToUpper(tokens[nextIdx]))
		right, afterRight, err := p.parseOperand(tokens, nextIdx+1)
		if err != nil {
			return nil, 0, err
		}
		left = &Condition{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
		nextIdx = afterRight
	}

	return left, nextIdx, nil
}

// Parse a single operand or parenthesized expression
func (p *SQLParser) parseOperand(tokens []string, idx int) (*Condition, int, error) {
	if idx >= len(tokens) {
		return nil, 0, fmt.Errorf("unexpected end of tokens")
	}

	// Handle parentheses
	if tokens[idx] == "(" {
		cond, nextIdx, err := p.parseConditionRecursive(tokens, idx+1)
		if err != nil {
			return nil, 0, err
		}
		if nextIdx >= len(tokens) || tokens[nextIdx] != ")" {
			return nil, 0, fmt.Errorf("expected closing parenthesis")
		}
		return cond, nextIdx + 1, nil
	}

	// Parse left operand
	if idx+2 >= len(tokens) {
		return nil, 0, fmt.Errorf("incomplete condition")
	}

	left := tokens[idx]
	operatorStr := tokens[idx+1]
	rightToken := tokens[idx+2]

	validOperators := []string{"=", "!=", "<", ">", "AND", "OR"}
	operatorValid := false
	for _, op := range validOperators {
		if operatorStr == op {
			operatorValid = true
			break
		}
	}

	if !operatorValid {
		return nil, 0, fmt.Errorf("missing or invalid operator in condition near '%s'", left)
	}

	// Check if rightToken is an operator (invalid)
	for _, op := range validOperators {
		if rightToken == op {
			return nil, 0, fmt.Errorf("unexpected operator '%s' after operator '%s'", rightToken, operatorStr)
		}
	}

	operator := Operator(operatorStr)
	var right interface{}

	// Parse right value
	if strings.HasPrefix(rightToken, "'") && strings.HasSuffix(rightToken, "'") {
		// String literal
		right = rightToken[1 : len(rightToken)-1]
	} else if num, err := strconv.ParseFloat(rightToken, 64); err == nil {
		// Number
		if strings.Contains(rightToken, ".") {
			right = num
		} else {
			right = int(num)
		}
	} else {
		// Column name or other identifier
		right = rightToken
	}

	return &Condition{
		Left:     left,
		Operator: operator,
		Right:    right,
	}, idx + 3, nil
}

func (p *SQLParser) evaluateCondition(row Row, condition *Condition) bool {
	// Handle logical operators
	if condition.Operator == OpAnd || condition.Operator == OpOr {
		leftCond, leftOk := condition.Left.(*Condition)
		rightCond, rightOk := condition.Right.(*Condition)

		if leftOk && rightOk {
			left := p.evaluateCondition(row, leftCond)
			right := p.evaluateCondition(row, rightCond)
			if condition.Operator == OpAnd {
				return left && right
			}
			return left || right
		}
		return false
	}

	// Handle comparison operators
	var leftVal interface{}
	switch v := condition.Left.(type) {
	case string:
		// Check if it's a column name
		if val, ok := row[v]; ok {
			leftVal = val
		} else {
			leftVal = v
		}
	case *Condition:
		leftVal = p.evaluateCondition(row, v)
	default:
		leftVal = v
	}

	var rightVal interface{}
	switch v := condition.Right.(type) {
	case string:
		// Check if it's a column name
		if val, ok := row[v]; ok {
			rightVal = val
		} else {
			rightVal = v
		}
	case *Condition:
		rightVal = p.evaluateCondition(row, v)
	default:
		rightVal = v
	}

	// Compare values
	switch condition.Operator {
	case OpEqual:
		return compareValues(leftVal, rightVal) == 0
	case OpNotEqual:
		return compareValues(leftVal, rightVal) != 0
	case OpLess:
		return compareValues(leftVal, rightVal) < 0
	case OpGreater:
		return compareValues(leftVal, rightVal) > 0
	default:
		return false
	}
}

// compareValues compares two values, returns -1 if left < right, 0 if equal, 1 if left > right
func compareValues(left, right interface{}) int {
	// Convert to numbers if possible
	leftNum, leftIsNum := toNumber(left)
	rightNum, rightIsNum := toNumber(right)

	if leftIsNum && rightIsNum {
		if leftNum < rightNum {
			return -1
		} else if leftNum > rightNum {
			return 1
		}
		return 0
	}

	// String comparison
	leftStr := fmt.Sprintf("%v", left)
	rightStr := fmt.Sprintf("%v", right)
	if leftStr < rightStr {
		return -1
	} else if leftStr > rightStr {
		return 1
	}
	return 0
}

func toNumber(val interface{}) (float64, bool) {
	switch v := val.(type) {
	case int:
		return float64(v), true
	case float64:
		return v, true
	case float32:
		return float64(v), true
	case string:
		if num, err := strconv.ParseFloat(v, 64); err == nil {
			return num, true
		}
	}
	return 0, false
}
