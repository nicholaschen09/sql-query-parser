package parser

import (
	"encoding/json"
	"fmt"
	"math"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// SQLParser parses and executes SQL queries
type SQLParser struct {
	tables map[string][]Row
}

// NewSQLParser creates a new SQL parser with the given tables
func NewSQLParser(tables map[string][]Row) *SQLParser {
	return &SQLParser{tables: tables}
}

// NewSQLParserFromData creates a parser from a single data array (backward compatible)
func NewSQLParserFromData(data []Row) *SQLParser {
	return &SQLParser{tables: map[string][]Row{"table": data}}
}

// Parse parses a SQL query string into a SelectQuery
func (p *SQLParser) Parse(query string) (*SelectQuery, error) {
	tokens := p.tokenize(query)
	return p.parseSelect(tokens)
}

// Execute executes a parsed query against the data
func (p *SQLParser) Execute(query *SelectQuery) ([]Row, error) {
	// 1. Get base table
	results := p.getTableData(query.Table)
	if results == nil {
		return nil, fmt.Errorf("table '%s' not found", query.Table)
	}
	results = copyRows(results)

	// 2. Apply JOINs
	if len(query.Joins) > 0 {
		var err error
		results, err = p.executeJoins(results, query.Table, query.Joins)
		if err != nil {
			return nil, err
		}
	}

	// 3. Apply WHERE
	if query.Where != nil {
		filtered := []Row{}
		for _, row := range results {
			if p.evaluateCondition(row, query.Where) {
				filtered = append(filtered, row)
			}
		}
		results = filtered
	}

	// 4. GROUP BY + aggregations
	hasAgg := hasAggregates(query.Columns)
	if len(query.GroupBy) > 0 || hasAgg {
		results = p.executeGroupBy(results, query.GroupBy, query.Columns, query.Having)
	} else {
		// 5. Column projection
		if len(query.Columns) > 0 && !(len(query.Columns) == 1 && !query.Columns[0].IsAggregate && query.Columns[0].Name == "*") {
			for i, row := range results {
				newRow := make(Row)
				for _, col := range query.Columns {
					if !col.IsAggregate {
						if val := p.resolveValue(row, col.Name); val != nil {
							newRow[col.Name] = val
						}
					}
				}
				results[i] = newRow
			}
		}
	}

	// 6. ORDER BY
	if len(query.OrderBy) > 0 {
		p.executeOrderBy(results, query.OrderBy)
	}

	// 7. LIMIT
	if query.Limit != nil {
		limit := *query.Limit
		if limit < len(results) {
			results = results[:limit]
		}
	}

	return results, nil
}

func (p *SQLParser) tokenize(query string) []string {
	tokens := []string{}
	q := strings.TrimSpace(query)
	q = strings.TrimSuffix(q, ";")
	i := 0

	for i < len(q) {
		ch := q[i]

		// Skip whitespace
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
			i++
			continue
		}

		// String literals
		if ch == '\'' {
			j := i + 1
			for j < len(q) && q[j] != '\'' {
				j++
			}
			tokens = append(tokens, q[i:j+1])
			i = j + 1
			continue
		}

		// Two-char operators
		if i+1 < len(q) {
			two := q[i : i+2]
			if two == ">=" || two == "<=" || two == "!=" {
				tokens = append(tokens, two)
				i += 2
				continue
			}
		}

		// Single-char punctuation
		if ch == '(' || ch == ')' || ch == ',' || ch == '=' || ch == '<' || ch == '>' || ch == '*' {
			tokens = append(tokens, string(ch))
			i++
			continue
		}

		// Words
		j := i
		for j < len(q) {
			c := q[j]
			if c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
				c == '(' || c == ')' || c == ',' || c == '=' || c == '<' || c == '>' || c == '*' || c == '\'' {
				break
			}
			if c == '!' && j+1 < len(q) && q[j+1] == '=' {
				break
			}
			j++
		}
		if j > i {
			tokens = append(tokens, q[i:j])
		}
		if j == i {
			i++
		} else {
			i = j
		}
	}
	return tokens
}

func (p *SQLParser) parseSelect(tokens []string) (*SelectQuery, error) {
	if len(tokens) == 0 || strings.ToUpper(tokens[0]) != "SELECT" {
		return nil, fmt.Errorf("query must start with SELECT")
	}

	// Find FROM (skip parenthesized expressions)
	depth := 0
	fromIndex := -1
	for i := 1; i < len(tokens); i++ {
		if tokens[i] == "(" {
			depth++
		} else if tokens[i] == ")" {
			depth--
		} else if depth == 0 && strings.ToUpper(tokens[i]) == "FROM" {
			fromIndex = i
			break
		}
	}
	if fromIndex == -1 {
		return nil, fmt.Errorf("expected FROM clause")
	}

	columns := p.parseColumnExprs(tokens[1:fromIndex])
	if len(columns) == 0 || (len(columns) == 1 && columns[0].Name == "") {
		return nil, fmt.Errorf("no columns specified in SELECT")
	}

	if fromIndex+1 >= len(tokens) {
		return nil, fmt.Errorf("expected table name after FROM")
	}
	table := tokens[fromIndex+1]

	query := &SelectQuery{
		Type:    "SELECT",
		Columns: columns,
		Table:   table,
	}

	idx := fromIndex + 2

	// Parse JOINs
	joins, afterJoins, err := p.parseJoins(tokens, idx)
	if err != nil {
		return nil, err
	}
	if len(joins) > 0 {
		query.Joins = joins
	}
	idx = afterJoins

	// Parse WHERE
	if idx < len(tokens) && strings.ToUpper(tokens[idx]) == "WHERE" {
		idx++
		cond, nextIdx, err := p.parseConditionRecursive(tokens, idx)
		if err != nil {
			return nil, err
		}
		query.Where = cond
		idx = nextIdx
	}

	// Parse GROUP BY
	if idx < len(tokens) && strings.ToUpper(tokens[idx]) == "GROUP" &&
		idx+1 < len(tokens) && strings.ToUpper(tokens[idx+1]) == "BY" {
		idx += 2
		groupBy := []string{}
		for idx < len(tokens) {
			upper := strings.ToUpper(tokens[idx])
			if upper == "HAVING" || upper == "ORDER" || upper == "LIMIT" {
				break
			}
			if tokens[idx] != "," {
				groupBy = append(groupBy, tokens[idx])
			}
			idx++
		}
		query.GroupBy = groupBy
	}

	// Parse HAVING
	if idx < len(tokens) && strings.ToUpper(tokens[idx]) == "HAVING" {
		idx++
		cond, nextIdx, err := p.parseConditionRecursive(tokens, idx)
		if err != nil {
			return nil, err
		}
		query.Having = cond
		idx = nextIdx
	}

	// Parse ORDER BY
	if idx < len(tokens) && strings.ToUpper(tokens[idx]) == "ORDER" &&
		idx+1 < len(tokens) && strings.ToUpper(tokens[idx+1]) == "BY" {
		idx += 2
		orderBy := []OrderByClause{}
		for idx < len(tokens) {
			if strings.ToUpper(tokens[idx]) == "LIMIT" {
				break
			}
			if tokens[idx] == "," {
				idx++
				continue
			}
			col := tokens[idx]
			idx++
			direction := "ASC"
			if idx < len(tokens) {
				dir := strings.ToUpper(tokens[idx])
				if dir == "ASC" || dir == "DESC" {
					direction = dir
					idx++
				}
			}
			orderBy = append(orderBy, OrderByClause{Column: col, Direction: direction})
		}
		query.OrderBy = orderBy
	}

	// Parse LIMIT
	if idx < len(tokens) && strings.ToUpper(tokens[idx]) == "LIMIT" {
		idx++
		if idx >= len(tokens) {
			return nil, fmt.Errorf("expected number after LIMIT")
		}
		limit, err := strconv.Atoi(tokens[idx])
		if err != nil || limit < 0 {
			return nil, fmt.Errorf("LIMIT must be a non-negative integer")
		}
		query.Limit = &limit
	}

	return query, nil
}

func (p *SQLParser) parseColumnExprs(tokens []string) []ColumnExpr {
	columns := []ColumnExpr{}
	aggFuncs := map[string]bool{"COUNT": true, "SUM": true, "AVG": true, "MIN": true, "MAX": true}
	i := 0

	for i < len(tokens) {
		if tokens[i] == "," || tokens[i] == "" {
			i++
			continue
		}

		upper := strings.ToUpper(tokens[i])
		if aggFuncs[upper] && i+1 < len(tokens) && tokens[i+1] == "(" {
			funcName := upper
			j := i + 2
			col := ""
			for j < len(tokens) && tokens[j] != ")" {
				if col != "" {
					col += " "
				}
				col += tokens[j]
				j++
			}
			j++ // skip )

			alias := ""
			if j < len(tokens) && strings.ToUpper(tokens[j]) == "AS" && j+1 < len(tokens) {
				alias = tokens[j+1]
				j += 2
			}

			if col == "" {
				col = "*"
			}
			columns = append(columns, ColumnExpr{IsAggregate: true, Name: col, Func: funcName, Alias: alias})
			i = j
		} else {
			name := strings.Trim(tokens[i], ",")
			if name != "" {
				columns = append(columns, ColumnExpr{Name: name})
			}
			i++
		}
	}
	return columns
}

func (p *SQLParser) parseJoins(tokens []string, idx int) ([]JoinClause, int, error) {
	joins := []JoinClause{}

	for idx < len(tokens) {
		upper := strings.ToUpper(tokens[idx])
		joinType := ""

		if upper == "JOIN" {
			joinType = "INNER"
			idx++
		} else if upper == "INNER" && idx+1 < len(tokens) && strings.ToUpper(tokens[idx+1]) == "JOIN" {
			joinType = "INNER"
			idx += 2
		} else if upper == "LEFT" {
			joinType = "LEFT"
			idx++
			if idx < len(tokens) && strings.ToUpper(tokens[idx]) == "JOIN" {
				idx++
			}
		} else if upper == "RIGHT" {
			joinType = "RIGHT"
			idx++
			if idx < len(tokens) && strings.ToUpper(tokens[idx]) == "JOIN" {
				idx++
			}
		} else {
			break
		}

		if idx >= len(tokens) {
			return nil, 0, fmt.Errorf("expected table name after JOIN")
		}
		table := tokens[idx]
		idx++

		if idx >= len(tokens) || strings.ToUpper(tokens[idx]) != "ON" {
			return nil, 0, fmt.Errorf("expected ON after JOIN table name")
		}
		idx++

		cond, nextIdx, err := p.parseConditionRecursive(tokens, idx)
		if err != nil {
			return nil, 0, err
		}
		joins = append(joins, JoinClause{JoinType: joinType, Table: table, On: cond})
		idx = nextIdx
	}

	return joins, idx, nil
}

// Parse OR (lowest precedence)
func (p *SQLParser) parseConditionRecursive(tokens []string, idx int) (*Condition, int, error) {
	left, nextIdx, err := p.parseAnd(tokens, idx)
	if err != nil {
		return nil, 0, err
	}

	for nextIdx < len(tokens) && strings.ToUpper(tokens[nextIdx]) == "OR" {
		right, afterRight, err := p.parseAnd(tokens, nextIdx+1)
		if err != nil {
			return nil, 0, err
		}
		left = &Condition{Left: left, Operator: OpOr, Right: right}
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
		right, afterRight, err := p.parseOperand(tokens, nextIdx+1)
		if err != nil {
			return nil, 0, err
		}
		left = &Condition{Left: left, Operator: OpAnd, Right: right}
		nextIdx = afterRight
	}

	return left, nextIdx, nil
}

// Parse a single operand
func (p *SQLParser) parseOperand(tokens []string, idx int) (*Condition, int, error) {
	if idx >= len(tokens) {
		return nil, 0, fmt.Errorf("unexpected end of tokens")
	}

	// Parenthesized expression
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

	left := tokens[idx]
	opIdx := idx + 1

	if opIdx >= len(tokens) {
		return nil, 0, fmt.Errorf("incomplete condition near '%s'", left)
	}

	// IS NULL / IS NOT NULL
	if strings.ToUpper(tokens[opIdx]) == "IS" {
		if opIdx+1 < len(tokens) && strings.ToUpper(tokens[opIdx+1]) == "NOT" &&
			opIdx+2 < len(tokens) && strings.ToUpper(tokens[opIdx+2]) == "NULL" {
			return &Condition{Left: left, Operator: OpIsNot, Right: nil}, opIdx + 3, nil
		}
		if opIdx+1 < len(tokens) && strings.ToUpper(tokens[opIdx+1]) == "NULL" {
			return &Condition{Left: left, Operator: OpIs, Right: nil}, opIdx + 2, nil
		}
	}

	// NOT IN / NOT LIKE
	hasNot := false
	if strings.ToUpper(tokens[opIdx]) == "NOT" {
		hasNot = true
		opIdx++
	}

	// IN
	if opIdx < len(tokens) && strings.ToUpper(tokens[opIdx]) == "IN" {
		opIdx++
		if opIdx >= len(tokens) || tokens[opIdx] != "(" {
			return nil, 0, fmt.Errorf("expected ( after IN")
		}
		opIdx++

		// Subquery?
		if opIdx < len(tokens) && strings.ToUpper(tokens[opIdx]) == "SELECT" {
			subTokens := []string{}
			subDepth := 1
			j := opIdx
			for j < len(tokens) && subDepth > 0 {
				if tokens[j] == "(" {
					subDepth++
				}
				if tokens[j] == ")" {
					subDepth--
					if subDepth == 0 {
						break
					}
				}
				subTokens = append(subTokens, tokens[j])
				j++
			}
			subQuery, err := p.parseSelect(subTokens)
			if err != nil {
				return nil, 0, fmt.Errorf("error parsing subquery: %v", err)
			}
			op := OpIn
			if hasNot {
				op = OpNotIn
			}
			return &Condition{Left: left, Operator: op, Right: subQuery}, j + 1, nil
		}

		// Value list
		values := []interface{}{}
		for opIdx < len(tokens) && tokens[opIdx] != ")" {
			if tokens[opIdx] != "," {
				values = append(values, parseValueToken(tokens[opIdx]))
			}
			opIdx++
		}
		opIdx++ // skip )
		op := OpIn
		if hasNot {
			op = OpNotIn
		}
		return &Condition{Left: left, Operator: op, Right: values}, opIdx, nil
	}

	// LIKE / NOT LIKE
	if opIdx < len(tokens) && strings.ToUpper(tokens[opIdx]) == "LIKE" {
		opIdx++
		if opIdx >= len(tokens) {
			return nil, 0, fmt.Errorf("expected pattern after LIKE")
		}
		right := parseValueToken(tokens[opIdx])
		op := OpLike
		if hasNot {
			op = OpNotLike
		}
		return &Condition{Left: left, Operator: op, Right: right}, opIdx + 1, nil
	}

	// Put back NOT if unused
	if hasNot {
		opIdx--
	}

	// Standard comparison
	if opIdx >= len(tokens) {
		return nil, 0, fmt.Errorf("incomplete condition near '%s'", left)
	}

	operatorStr := tokens[opIdx]
	validOps := map[string]Operator{
		"=": OpEqual, "!=": OpNotEqual, "<": OpLess, ">": OpGreater,
		"<=": OpLessEq, ">=": OpGreaterEq,
	}

	op, ok := validOps[operatorStr]
	if !ok {
		return nil, 0, fmt.Errorf("missing or invalid operator in condition near '%s'", left)
	}

	if opIdx+1 >= len(tokens) {
		return nil, 0, fmt.Errorf("missing right-hand side in condition near '%s %s'", left, operatorStr)
	}

	rightToken := tokens[opIdx+1]
	right := parseValueToken(rightToken)

	return &Condition{Left: left, Operator: op, Right: right}, opIdx + 2, nil
}

func parseValueToken(token string) interface{} {
	if strings.ToUpper(token) == "NULL" {
		return nil
	}
	if strings.HasPrefix(token, "'") && strings.HasSuffix(token, "'") && len(token) >= 2 {
		return token[1 : len(token)-1]
	}
	if strings.Contains(token, ".") {
		if num, err := strconv.ParseFloat(token, 64); err == nil {
			return num
		}
	} else {
		if num, err := strconv.Atoi(token); err == nil {
			return num
		}
		if num, err := strconv.ParseFloat(token, 64); err == nil {
			return num
		}
	}
	return token
}

func (p *SQLParser) getTableData(name string) []Row {
	if data, ok := p.tables[name]; ok {
		return data
	}
	lower := strings.ToLower(name)
	for k, v := range p.tables {
		if strings.ToLower(k) == lower {
			return v
		}
	}
	if data, ok := p.tables["table"]; ok {
		return data
	}
	if len(p.tables) == 1 {
		for _, v := range p.tables {
			return v
		}
	}
	return nil
}

func (p *SQLParser) executeJoins(leftRows []Row, leftTable string, joins []JoinClause) ([]Row, error) {
	results := leftRows

	for _, join := range joins {
		rightData := p.getTableData(join.Table)
		if rightData == nil {
			return nil, fmt.Errorf("table '%s' not found for JOIN", join.Table)
		}

		joined := []Row{}

		if join.JoinType == "RIGHT" {
			for _, rightRow := range rightData {
				matched := false
				for _, leftRow := range results {
					merged := mergeRows(leftRow, rightRow, leftTable, join.Table)
					if p.evaluateCondition(merged, join.On) {
						joined = append(joined, merged)
						matched = true
					}
				}
				if !matched {
					merged := make(Row)
					if len(results) > 0 {
						for key := range results[0] {
							if !strings.Contains(key, ".") {
								merged[key] = nil
								merged[leftTable+"."+key] = nil
							}
						}
					}
					for k, v := range rightRow {
						merged[k] = v
						merged[join.Table+"."+k] = v
					}
					joined = append(joined, merged)
				}
			}
		} else {
			for _, leftRow := range results {
				matched := false
				for _, rightRow := range rightData {
					merged := mergeRows(leftRow, rightRow, leftTable, join.Table)
					if p.evaluateCondition(merged, join.On) {
						joined = append(joined, merged)
						matched = true
					}
				}
				if !matched && join.JoinType == "LEFT" {
					merged := make(Row)
					for k, v := range leftRow {
						merged[k] = v
						if !strings.Contains(k, ".") {
							merged[leftTable+"."+k] = v
						}
					}
					if len(rightData) > 0 {
						for key := range rightData[0] {
							merged[key] = nil
							merged[join.Table+"."+key] = nil
						}
					}
					joined = append(joined, merged)
				}
			}
		}

		results = joined
	}

	return results, nil
}

func mergeRows(leftRow, rightRow Row, leftTable, rightTable string) Row {
	merged := make(Row)
	for k, v := range leftRow {
		merged[k] = v
		if !strings.Contains(k, ".") {
			merged[leftTable+"."+k] = v
		}
	}
	for k, v := range rightRow {
		merged[k] = v
		if !strings.Contains(k, ".") {
			merged[rightTable+"."+k] = v
		}
	}
	return merged
}

func (p *SQLParser) executeGroupBy(rows []Row, groupBy []string, columns []ColumnExpr, having *Condition) []Row {
	groups := map[string][]Row{}
	order := []string{}

	if len(groupBy) == 0 {
		groups["__all__"] = rows
		order = []string{"__all__"}
	} else {
		for _, row := range rows {
			parts := []string{}
			for _, col := range groupBy {
				val := p.resolveValue(row, col)
				b, _ := json.Marshal(val)
				parts = append(parts, string(b))
			}
			key := strings.Join(parts, "|")
			if _, ok := groups[key]; !ok {
				order = append(order, key)
			}
			groups[key] = append(groups[key], row)
		}
	}

	results := []Row{}
	for _, key := range order {
		groupRows := groups[key]
		newRow := make(Row)

		for _, col := range groupBy {
			newRow[col] = p.resolveValue(groupRows[0], col)
		}

		for _, col := range columns {
			if col.IsAggregate {
				alias := col.Alias
				if alias == "" {
					alias = col.Func + "(" + col.Name + ")"
				}
				newRow[alias] = p.computeAggregate(col.Func, col.Name, groupRows)
			} else if col.Name != "*" && !containsString(groupBy, col.Name) {
				newRow[col.Name] = p.resolveValue(groupRows[0], col.Name)
			}
		}

		results = append(results, newRow)
	}

	if having != nil {
		filtered := []Row{}
		for _, row := range results {
			if p.evaluateCondition(row, having) {
				filtered = append(filtered, row)
			}
		}
		results = filtered
	}

	return results
}

func (p *SQLParser) computeAggregate(funcName, column string, rows []Row) interface{} {
	switch funcName {
	case "COUNT":
		if column == "*" {
			return len(rows)
		}
		count := 0
		for _, r := range rows {
			if p.resolveValue(r, column) != nil {
				count++
			}
		}
		return count
	case "SUM":
		sum := 0.0
		for _, r := range rows {
			if n, ok := toNumber(p.resolveValue(r, column)); ok {
				sum += n
			}
		}
		if sum == math.Trunc(sum) {
			return int(sum)
		}
		return sum
	case "AVG":
		sum := 0.0
		count := 0
		for _, r := range rows {
			if n, ok := toNumber(p.resolveValue(r, column)); ok {
				sum += n
				count++
			}
		}
		if count == 0 {
			return nil
		}
		avg := sum / float64(count)
		if avg == math.Trunc(avg) {
			return int(avg)
		}
		return avg
	case "MIN":
		var min interface{}
		for _, r := range rows {
			v := p.resolveValue(r, column)
			if v == nil {
				continue
			}
			if min == nil || compareValues(v, min) < 0 {
				min = v
			}
		}
		return min
	case "MAX":
		var max interface{}
		for _, r := range rows {
			v := p.resolveValue(r, column)
			if v == nil {
				continue
			}
			if max == nil || compareValues(v, max) > 0 {
				max = v
			}
		}
		return max
	}
	return nil
}

func (p *SQLParser) executeOrderBy(rows []Row, orderBy []OrderByClause) {
	sort.SliceStable(rows, func(i, j int) bool {
		for _, ob := range orderBy {
			av := p.resolveValue(rows[i], ob.Column)
			bv := p.resolveValue(rows[j], ob.Column)
			cmp := compareValues(av, bv)
			if ob.Direction == "DESC" {
				cmp = -cmp
			}
			if cmp != 0 {
				return cmp < 0
			}
		}
		return false
	})
}

func (p *SQLParser) evaluateCondition(row Row, condition *Condition) bool {
	// Logical operators
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

	// Resolve left value
	var leftVal interface{}
	switch v := condition.Left.(type) {
	case string:
		resolved := p.resolveValue(row, v)
		if resolved != nil {
			leftVal = resolved
		} else {
			leftVal = v
		}
	case *Condition:
		leftVal = p.evaluateCondition(row, v)
	default:
		leftVal = v
	}

	switch condition.Operator {
	case OpIs:
		return leftVal == nil
	case OpIsNot:
		return leftVal != nil
	case OpIn:
		if leftVal == nil {
			return false
		}
		switch rv := condition.Right.(type) {
		case []interface{}:
			for _, v := range rv {
				if compareValues(leftVal, v) == 0 {
					return true
				}
			}
			return false
		case *SelectQuery:
			subResults, err := p.Execute(rv)
			if err != nil {
				return false
			}
			for _, r := range subResults {
				for _, v := range r {
					if compareValues(leftVal, v) == 0 {
						return true
					}
					break
				}
			}
			return false
		}
		return false
	case OpNotIn:
		if leftVal == nil {
			return false
		}
		switch rv := condition.Right.(type) {
		case []interface{}:
			for _, v := range rv {
				if compareValues(leftVal, v) == 0 {
					return false
				}
			}
			return true
		case *SelectQuery:
			subResults, err := p.Execute(rv)
			if err != nil {
				return true
			}
			for _, r := range subResults {
				for _, v := range r {
					if compareValues(leftVal, v) == 0 {
						return false
					}
					break
				}
			}
			return true
		}
		return true
	case OpLike, OpNotLike:
		if leftVal == nil {
			return false
		}
		pattern := fmt.Sprintf("%v", condition.Right)
		pattern = strings.ReplaceAll(pattern, "%", ".*")
		pattern = strings.ReplaceAll(pattern, "_", ".")
		re, err := regexp.Compile("(?i)^" + pattern + "$")
		if err != nil {
			return false
		}
		match := re.MatchString(fmt.Sprintf("%v", leftVal))
		if condition.Operator == OpNotLike {
			return !match
		}
		return match
	default:
		// Standard comparison
		var rightVal interface{}
		switch v := condition.Right.(type) {
		case string:
			resolved := p.resolveValue(row, v)
			if resolved != nil {
				rightVal = resolved
			} else {
				rightVal = v
			}
		default:
			rightVal = v
		}

		if leftVal == nil || rightVal == nil {
			if condition.Operator == OpEqual {
				return leftVal == nil && rightVal == nil
			}
			if condition.Operator == OpNotEqual {
				return !(leftVal == nil && rightVal == nil)
			}
			return false
		}

		cmp := compareValues(leftVal, rightVal)
		switch condition.Operator {
		case OpEqual:
			return cmp == 0
		case OpNotEqual:
			return cmp != 0
		case OpLess:
			return cmp < 0
		case OpGreater:
			return cmp > 0
		case OpLessEq:
			return cmp <= 0
		case OpGreaterEq:
			return cmp >= 0
		}
	}
	return false
}

func (p *SQLParser) resolveValue(row Row, path string) interface{} {
	if path == "" {
		return nil
	}

	// Direct key match
	if val, ok := row[path]; ok {
		return val
	}

	// Dot notation
	if strings.Contains(path, ".") {
		parts := strings.Split(path, ".")

		// Try progressively longer prefixes
		for i := 1; i < len(parts); i++ {
			prefix := strings.Join(parts[:i], ".")
			if val, ok := row[prefix]; ok {
				current := val
				for j := i; j < len(parts); j++ {
					m, ok := current.(map[string]interface{})
					if !ok {
						current = nil
						break
					}
					current = m[parts[j]]
				}
				if current != nil {
					return current
				}
			}
		}

		// Full traversal
		var current interface{} = map[string]interface{}(row)
		for _, part := range parts {
			m, ok := current.(map[string]interface{})
			if !ok {
				return nil
			}
			current = m[part]
		}
		return current
	}

	return nil
}

// compareValues compares two values
func compareValues(left, right interface{}) int {
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

func copyRows(rows []Row) []Row {
	result := make([]Row, len(rows))
	for i, row := range rows {
		newRow := make(Row)
		for k, v := range row {
			newRow[k] = v
		}
		result[i] = newRow
	}
	return result
}

func hasAggregates(columns []ColumnExpr) bool {
	for _, c := range columns {
		if c.IsAggregate {
			return true
		}
	}
	return false
}

func containsString(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}
