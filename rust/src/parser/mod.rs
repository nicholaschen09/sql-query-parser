pub mod types;

use std::collections::HashMap;
use regex::Regex;
use types::*;

pub struct SQLParser {
    tables: HashMap<String, Vec<Row>>,
}

impl SQLParser {
    pub fn new(data: Vec<Row>) -> Self {
        let mut tables = HashMap::new();
        tables.insert("table".to_string(), data);
        SQLParser { tables }
    }

    pub fn new_with_tables(tables: HashMap<String, Vec<Row>>) -> Self {
        SQLParser { tables }
    }

    pub fn parse(&self, query: &str) -> Result<SelectQuery, String> {
        let tokens = self.tokenize(query);
        self.parse_select(&tokens)
    }

    pub fn execute(&self, query: &SelectQuery) -> Result<Vec<Row>, String> {
        // 1. Get base table
        let mut results = self.get_table_data(&query.table)
            .ok_or_else(|| format!("table '{}' not found", query.table))?
            .clone();

        // 2. JOINs
        if !query.joins.is_empty() {
            results = self.execute_joins(results, &query.table, &query.joins)?;
        }

        // 3. WHERE
        if let Some(ref cond) = query.r#where {
            results = results.into_iter()
                .filter(|row| self.evaluate_condition(row, cond))
                .collect();
        }

        // 4. GROUP BY + aggregations
        let has_agg = query.column_exprs.iter().any(|c| c.is_aggregate);
        if !query.group_by.is_empty() || has_agg {
            results = self.execute_group_by(&results, &query.group_by, &query.column_exprs, &query.having);
        } else {
            // 5. Column projection
            if !query.column_exprs.is_empty() && !(query.column_exprs.len() == 1 && !query.column_exprs[0].is_aggregate && query.column_exprs[0].name == "*") {
                results = results.into_iter().map(|row| {
                    let mut new_row = Row::new();
                    for col in &query.column_exprs {
                        if !col.is_aggregate {
                            if let Some(val) = self.resolve_value(&row, &col.name) {
                                new_row.insert(col.name.clone(), val);
                            }
                        }
                    }
                    new_row
                }).collect();
            }
        }

        // 6. ORDER BY
        if !query.order_by.is_empty() {
            self.execute_order_by(&mut results, &query.order_by);
        }

        // 7. LIMIT
        if let Some(limit) = query.limit {
            results.truncate(limit);
        }

        Ok(results)
    }

    fn tokenize(&self, query: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let q = query.trim().trim_end_matches(';');
        let chars: Vec<char> = q.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            let ch = chars[i];

            if ch.is_whitespace() { i += 1; continue; }

            // String literals
            if ch == '\'' {
                let mut j = i + 1;
                while j < chars.len() && chars[j] != '\'' { j += 1; }
                tokens.push(chars[i..=j.min(chars.len()-1)].iter().collect());
                i = j + 1;
                continue;
            }

            // Two-char operators
            if i + 1 < chars.len() {
                let two: String = chars[i..i+2].iter().collect();
                if two == ">=" || two == "<=" || two == "!=" {
                    tokens.push(two);
                    i += 2;
                    continue;
                }
            }

            // Single-char punctuation
            if "(),=<>*".contains(ch) {
                tokens.push(ch.to_string());
                i += 1;
                continue;
            }

            // Words
            let mut j = i;
            while j < chars.len() {
                let c = chars[j];
                if c.is_whitespace() || "(),=<>*'".contains(c) { break; }
                if c == '!' && j + 1 < chars.len() && chars[j+1] == '=' { break; }
                j += 1;
            }
            if j > i {
                tokens.push(chars[i..j].iter().collect());
            }
            i = if j == i { i + 1 } else { j };
        }
        tokens
    }

    fn parse_select(&self, tokens: &[String]) -> Result<SelectQuery, String> {
        if tokens.is_empty() || tokens[0].to_uppercase() != "SELECT" {
            return Err("query must start with SELECT".to_string());
        }

        // Find FROM
        let mut depth = 0i32;
        let mut from_index = None;
        for i in 1..tokens.len() {
            if tokens[i] == "(" { depth += 1; }
            else if tokens[i] == ")" { depth -= 1; }
            else if depth == 0 && tokens[i].to_uppercase() == "FROM" {
                from_index = Some(i);
                break;
            }
        }
        let from_index = from_index.ok_or("expected FROM clause")?;

        let column_exprs = self.parse_column_exprs(&tokens[1..from_index]);
        if column_exprs.is_empty() {
            return Err("no columns specified in SELECT".to_string());
        }

        if from_index + 1 >= tokens.len() {
            return Err("expected table name after FROM".to_string());
        }
        let table = tokens[from_index + 1].clone();
        let columns: Vec<String> = column_exprs.iter().map(|c| c.name.clone()).collect();

        let mut idx = from_index + 2;

        // JOINs
        let (joins, after_joins) = self.parse_joins(tokens, idx)?;
        idx = after_joins;

        // WHERE
        let mut where_clause = None;
        if idx < tokens.len() && tokens[idx].to_uppercase() == "WHERE" {
            idx += 1;
            let (cond, next_idx) = self.parse_condition_recursive(tokens, idx)?;
            where_clause = Some(cond);
            idx = next_idx;
        }

        // GROUP BY
        let mut group_by = Vec::new();
        if idx < tokens.len() && tokens[idx].to_uppercase() == "GROUP"
            && idx + 1 < tokens.len() && tokens[idx + 1].to_uppercase() == "BY" {
            idx += 2;
            while idx < tokens.len() {
                let upper = tokens[idx].to_uppercase();
                if upper == "HAVING" || upper == "ORDER" || upper == "LIMIT" { break; }
                if tokens[idx] != "," { group_by.push(tokens[idx].clone()); }
                idx += 1;
            }
        }

        // HAVING
        let mut having = None;
        if idx < tokens.len() && tokens[idx].to_uppercase() == "HAVING" {
            idx += 1;
            let (cond, next_idx) = self.parse_condition_recursive(tokens, idx)?;
            having = Some(cond);
            idx = next_idx;
        }

        // ORDER BY
        let mut order_by = Vec::new();
        if idx < tokens.len() && tokens[idx].to_uppercase() == "ORDER"
            && idx + 1 < tokens.len() && tokens[idx + 1].to_uppercase() == "BY" {
            idx += 2;
            while idx < tokens.len() {
                if tokens[idx].to_uppercase() == "LIMIT" { break; }
                if tokens[idx] == "," { idx += 1; continue; }
                let col = tokens[idx].clone();
                idx += 1;
                let mut direction = "ASC".to_string();
                if idx < tokens.len() {
                    let dir = tokens[idx].to_uppercase();
                    if dir == "ASC" || dir == "DESC" {
                        direction = dir;
                        idx += 1;
                    }
                }
                order_by.push(OrderByClause { column: col, direction });
            }
        }

        // LIMIT
        let mut limit = None;
        if idx < tokens.len() && tokens[idx].to_uppercase() == "LIMIT" {
            idx += 1;
            if idx >= tokens.len() {
                return Err("expected number after LIMIT".to_string());
            }
            let n: usize = tokens[idx].parse()
                .map_err(|_| "LIMIT must be a non-negative integer".to_string())?;
            limit = Some(n);
        }

        Ok(SelectQuery {
            query_type: "SELECT".to_string(),
            columns,
            table,
            column_exprs,
            joins,
            r#where: where_clause,
            group_by,
            having,
            order_by,
            limit,
        })
    }

    fn parse_column_exprs(&self, tokens: &[String]) -> Vec<ColumnExpr> {
        let mut columns = Vec::new();
        let agg_funcs = ["COUNT", "SUM", "AVG", "MIN", "MAX"];
        let mut i = 0;

        while i < tokens.len() {
            if tokens[i] == "," || tokens[i].is_empty() { i += 1; continue; }

            let upper = tokens[i].to_uppercase();
            if agg_funcs.contains(&upper.as_str()) && i + 1 < tokens.len() && tokens[i + 1] == "(" {
                let func_name = upper;
                let mut j = i + 2;
                let mut col = String::new();
                while j < tokens.len() && tokens[j] != ")" {
                    if !col.is_empty() { col.push(' '); }
                    col.push_str(&tokens[j]);
                    j += 1;
                }
                j += 1; // skip )

                let mut alias = String::new();
                if j < tokens.len() && tokens[j].to_uppercase() == "AS" && j + 1 < tokens.len() {
                    alias = tokens[j + 1].clone();
                    j += 2;
                }

                if col.is_empty() { col = "*".to_string(); }
                columns.push(ColumnExpr { is_aggregate: true, name: col, func_name, alias });
                i = j;
            } else {
                let name = tokens[i].trim_matches(',').to_string();
                if !name.is_empty() {
                    columns.push(ColumnExpr { is_aggregate: false, name, func_name: String::new(), alias: String::new() });
                }
                i += 1;
            }
        }
        columns
    }

    fn parse_joins(&self, tokens: &[String], mut idx: usize) -> Result<(Vec<JoinClause>, usize), String> {
        let mut joins = Vec::new();

        while idx < tokens.len() {
            let upper = tokens[idx].to_uppercase();
            let join_type;

            if upper == "JOIN" {
                join_type = "INNER".to_string();
                idx += 1;
            } else if upper == "INNER" && idx + 1 < tokens.len() && tokens[idx + 1].to_uppercase() == "JOIN" {
                join_type = "INNER".to_string();
                idx += 2;
            } else if upper == "LEFT" {
                join_type = "LEFT".to_string();
                idx += 1;
                if idx < tokens.len() && tokens[idx].to_uppercase() == "JOIN" { idx += 1; }
            } else if upper == "RIGHT" {
                join_type = "RIGHT".to_string();
                idx += 1;
                if idx < tokens.len() && tokens[idx].to_uppercase() == "JOIN" { idx += 1; }
            } else {
                break;
            }

            if idx >= tokens.len() { return Err("expected table name after JOIN".to_string()); }
            let table = tokens[idx].clone();
            idx += 1;

            if idx >= tokens.len() || tokens[idx].to_uppercase() != "ON" {
                return Err("expected ON after JOIN table name".to_string());
            }
            idx += 1;

            let (cond, next_idx) = self.parse_condition_recursive(tokens, idx)?;
            joins.push(JoinClause { join_type, table, on: cond });
            idx = next_idx;
        }

        Ok((joins, idx))
    }

    fn parse_condition_recursive(&self, tokens: &[String], idx: usize) -> Result<(Condition, usize), String> {
        let (mut left, mut next_idx) = self.parse_and(tokens, idx)?;

        while next_idx < tokens.len() && tokens[next_idx].to_uppercase() == "OR" {
            let (right, after_right) = self.parse_and(tokens, next_idx + 1)?;
            left = Condition {
                left: ConditionOperand::Condition(Box::new(left)),
                operator: Operator::Or,
                right: ConditionOperand::Condition(Box::new(right)),
            };
            next_idx = after_right;
        }

        Ok((left, next_idx))
    }

    fn parse_and(&self, tokens: &[String], idx: usize) -> Result<(Condition, usize), String> {
        let (mut left, mut next_idx) = self.parse_operand(tokens, idx)?;

        while next_idx < tokens.len() && tokens[next_idx].to_uppercase() == "AND" {
            let (right, after_right) = self.parse_operand(tokens, next_idx + 1)?;
            left = Condition {
                left: ConditionOperand::Condition(Box::new(left)),
                operator: Operator::And,
                right: ConditionOperand::Condition(Box::new(right)),
            };
            next_idx = after_right;
        }

        Ok((left, next_idx))
    }

    fn parse_operand(&self, tokens: &[String], idx: usize) -> Result<(Condition, usize), String> {
        if idx >= tokens.len() { return Err("unexpected end of tokens".to_string()); }

        // Parentheses
        if tokens[idx] == "(" {
            let (cond, next_idx) = self.parse_condition_recursive(tokens, idx + 1)?;
            if next_idx >= tokens.len() || tokens[next_idx] != ")" {
                return Err("expected closing parenthesis".to_string());
            }
            return Ok((cond, next_idx + 1));
        }

        let left = tokens[idx].clone();
        let mut op_idx = idx + 1;

        if op_idx >= tokens.len() {
            return Err(format!("incomplete condition near '{}'", left));
        }

        // IS NULL / IS NOT NULL
        if tokens[op_idx].to_uppercase() == "IS" {
            if op_idx + 1 < tokens.len() && tokens[op_idx + 1].to_uppercase() == "NOT"
                && op_idx + 2 < tokens.len() && tokens[op_idx + 2].to_uppercase() == "NULL" {
                return Ok((Condition {
                    left: ConditionOperand::Literal(left),
                    operator: Operator::IsNot,
                    right: ConditionOperand::Null,
                }, op_idx + 3));
            }
            if op_idx + 1 < tokens.len() && tokens[op_idx + 1].to_uppercase() == "NULL" {
                return Ok((Condition {
                    left: ConditionOperand::Literal(left),
                    operator: Operator::Is,
                    right: ConditionOperand::Null,
                }, op_idx + 2));
            }
        }

        // NOT IN / NOT LIKE
        let mut has_not = false;
        if tokens[op_idx].to_uppercase() == "NOT" {
            has_not = true;
            op_idx += 1;
        }

        // IN
        if op_idx < tokens.len() && tokens[op_idx].to_uppercase() == "IN" {
            op_idx += 1;
            if op_idx >= tokens.len() || tokens[op_idx] != "(" {
                return Err("expected ( after IN".to_string());
            }
            op_idx += 1;

            // Subquery
            if op_idx < tokens.len() && tokens[op_idx].to_uppercase() == "SELECT" {
                let mut sub_tokens = Vec::new();
                let mut sub_depth = 1i32;
                let mut j = op_idx;
                while j < tokens.len() && sub_depth > 0 {
                    if tokens[j] == "(" { sub_depth += 1; }
                    if tokens[j] == ")" { sub_depth -= 1; if sub_depth == 0 { break; } }
                    sub_tokens.push(tokens[j].clone());
                    j += 1;
                }
                let sub_query = self.parse_select(&sub_tokens)?;
                let op = if has_not { Operator::NotIn } else { Operator::In };
                return Ok((Condition {
                    left: ConditionOperand::Literal(left),
                    operator: op,
                    right: ConditionOperand::SubQuery(Box::new(sub_query)),
                }, j + 1));
            }

            // Value list
            let mut values = Vec::new();
            while op_idx < tokens.len() && tokens[op_idx] != ")" {
                if tokens[op_idx] != "," {
                    values.push(parse_value_token(&tokens[op_idx]));
                }
                op_idx += 1;
            }
            op_idx += 1; // skip )
            let op = if has_not { Operator::NotIn } else { Operator::In };
            return Ok((Condition {
                left: ConditionOperand::Literal(left),
                operator: op,
                right: ConditionOperand::ValueList(values),
            }, op_idx));
        }

        // LIKE
        if op_idx < tokens.len() && tokens[op_idx].to_uppercase() == "LIKE" {
            op_idx += 1;
            if op_idx >= tokens.len() { return Err("expected pattern after LIKE".to_string()); }
            let right = tokens[op_idx].clone();
            let op = if has_not { Operator::NotLike } else { Operator::Like };
            return Ok((Condition {
                left: ConditionOperand::Literal(left),
                operator: op,
                right: ConditionOperand::Literal(right),
            }, op_idx + 1));
        }

        if has_not { op_idx -= 1; }

        // Standard comparison
        if op_idx >= tokens.len() {
            return Err(format!("incomplete condition near '{}'", left));
        }

        let operator = match tokens[op_idx].as_str() {
            "=" => Operator::Equal,
            "!=" => Operator::NotEqual,
            "<" => Operator::Less,
            ">" => Operator::Greater,
            "<=" => Operator::LessEq,
            ">=" => Operator::GreaterEq,
            _ => return Err(format!("missing or invalid operator in condition near '{}'", left)),
        };

        if op_idx + 1 >= tokens.len() {
            return Err(format!("missing right-hand side in condition near '{}'", left));
        }

        let right_token = tokens[op_idx + 1].clone();
        let right_operand = if right_token.to_uppercase() == "NULL" {
            ConditionOperand::Null
        } else {
            ConditionOperand::Literal(right_token)
        };

        Ok((Condition {
            left: ConditionOperand::Literal(left),
            operator,
            right: right_operand,
        }, op_idx + 2))
    }

    fn get_table_data(&self, name: &str) -> Option<&Vec<Row>> {
        if let Some(data) = self.tables.get(name) { return Some(data); }
        let lower = name.to_lowercase();
        for (k, v) in &self.tables {
            if k.to_lowercase() == lower { return Some(v); }
        }
        if let Some(data) = self.tables.get("table") { return Some(data); }
        if self.tables.len() == 1 {
            return self.tables.values().next();
        }
        None
    }

    fn execute_joins(&self, left_rows: Vec<Row>, left_table: &str, joins: &[JoinClause]) -> Result<Vec<Row>, String> {
        let mut results = left_rows;

        for join in joins {
            let right_data = self.get_table_data(&join.table)
                .ok_or_else(|| format!("table '{}' not found for JOIN", join.table))?
                .clone();

            let mut joined = Vec::new();

            if join.join_type == "RIGHT" {
                for right_row in &right_data {
                    let mut matched = false;
                    for left_row in &results {
                        let merged = merge_rows(left_row, right_row, left_table, &join.table);
                        if self.evaluate_condition(&merged, &join.on) {
                            joined.push(merged);
                            matched = true;
                        }
                    }
                    if !matched {
                        let mut merged = Row::new();
                        if let Some(first) = results.first() {
                            for key in first.keys() {
                                if !key.contains('.') {
                                    merged.insert(key.clone(), Value::Null);
                                    merged.insert(format!("{}.{}", left_table, key), Value::Null);
                                }
                            }
                        }
                        for (k, v) in right_row {
                            merged.insert(k.clone(), v.clone());
                            merged.insert(format!("{}.{}", join.table, k), v.clone());
                        }
                        joined.push(merged);
                    }
                }
            } else {
                for left_row in &results {
                    let mut matched = false;
                    for right_row in &right_data {
                        let merged = merge_rows(left_row, right_row, left_table, &join.table);
                        if self.evaluate_condition(&merged, &join.on) {
                            joined.push(merged);
                            matched = true;
                        }
                    }
                    if !matched && join.join_type == "LEFT" {
                        let mut merged = Row::new();
                        for (k, v) in left_row {
                            merged.insert(k.clone(), v.clone());
                            if !k.contains('.') {
                                merged.insert(format!("{}.{}", left_table, k), v.clone());
                            }
                        }
                        if let Some(first) = right_data.first() {
                            for key in first.keys() {
                                merged.insert(key.clone(), Value::Null);
                                merged.insert(format!("{}.{}", join.table, key), Value::Null);
                            }
                        }
                        joined.push(merged);
                    }
                }
            }

            results = joined;
        }

        Ok(results)
    }

    fn execute_group_by(&self, rows: &[Row], group_by: &[String], columns: &[ColumnExpr], having: &Option<Condition>) -> Vec<Row> {
        let mut groups: Vec<(String, Vec<Row>)> = Vec::new();
        let mut group_map: HashMap<String, usize> = HashMap::new();

        if group_by.is_empty() {
            groups.push(("__all__".to_string(), rows.to_vec()));
        } else {
            for row in rows {
                let key: String = group_by.iter()
                    .map(|col| format!("{:?}", self.resolve_value(row, col)))
                    .collect::<Vec<_>>()
                    .join("|");
                if let Some(&idx) = group_map.get(&key) {
                    groups[idx].1.push(row.clone());
                } else {
                    group_map.insert(key.clone(), groups.len());
                    groups.push((key, vec![row.clone()]));
                }
            }
        }

        let mut results = Vec::new();
        for (_, group_rows) in &groups {
            let mut new_row = Row::new();

            for col in group_by {
                if let Some(val) = self.resolve_value(&group_rows[0], col) {
                    new_row.insert(col.clone(), val);
                }
            }

            for col in columns {
                if col.is_aggregate {
                    let alias = if col.alias.is_empty() {
                        format!("{}({})", col.func_name, col.name)
                    } else {
                        col.alias.clone()
                    };
                    new_row.insert(alias, self.compute_aggregate(&col.func_name, &col.name, group_rows));
                } else if col.name != "*" && !group_by.contains(&col.name) {
                    if let Some(val) = self.resolve_value(&group_rows[0], &col.name) {
                        new_row.insert(col.name.clone(), val);
                    }
                }
            }

            results.push(new_row);
        }

        if let Some(ref having_cond) = having {
            results = results.into_iter()
                .filter(|row| self.evaluate_condition(row, having_cond))
                .collect();
        }

        results
    }

    fn compute_aggregate(&self, func_name: &str, column: &str, rows: &[Row]) -> Value {
        match func_name {
            "COUNT" => {
                if column == "*" {
                    Value::Number(rows.len() as f64)
                } else {
                    let count = rows.iter()
                        .filter(|r| self.resolve_value(r, column).map_or(false, |v| v != Value::Null))
                        .count();
                    Value::Number(count as f64)
                }
            }
            "SUM" => {
                let sum: f64 = rows.iter()
                    .filter_map(|r| self.resolve_value(r, column).and_then(|v| to_number(&v)))
                    .sum();
                Value::Number(sum)
            }
            "AVG" => {
                let nums: Vec<f64> = rows.iter()
                    .filter_map(|r| self.resolve_value(r, column).and_then(|v| to_number(&v)))
                    .collect();
                if nums.is_empty() { Value::Null } else {
                    Value::Number(nums.iter().sum::<f64>() / nums.len() as f64)
                }
            }
            "MIN" => {
                let mut min: Option<Value> = None;
                for r in rows {
                    if let Some(v) = self.resolve_value(r, column) {
                        if v == Value::Null { continue; }
                        if min.is_none() || compare_values(&v, min.as_ref().unwrap()) < 0 {
                            min = Some(v);
                        }
                    }
                }
                min.unwrap_or(Value::Null)
            }
            "MAX" => {
                let mut max: Option<Value> = None;
                for r in rows {
                    if let Some(v) = self.resolve_value(r, column) {
                        if v == Value::Null { continue; }
                        if max.is_none() || compare_values(&v, max.as_ref().unwrap()) > 0 {
                            max = Some(v);
                        }
                    }
                }
                max.unwrap_or(Value::Null)
            }
            _ => Value::Null,
        }
    }

    fn execute_order_by(&self, rows: &mut Vec<Row>, order_by: &[OrderByClause]) {
        rows.sort_by(|a, b| {
            for ob in order_by {
                let av = self.resolve_value(a, &ob.column);
                let bv = self.resolve_value(b, &ob.column);
                let av_ref = av.as_ref().unwrap_or(&Value::Null);
                let bv_ref = bv.as_ref().unwrap_or(&Value::Null);
                let mut cmp = compare_values(av_ref, bv_ref);
                if ob.direction == "DESC" { cmp = -cmp; }
                if cmp != 0 { return if cmp < 0 { std::cmp::Ordering::Less } else { std::cmp::Ordering::Greater }; }
            }
            std::cmp::Ordering::Equal
        });
    }

    fn evaluate_condition(&self, row: &Row, condition: &Condition) -> bool {
        // Logical operators
        if condition.operator == Operator::And || condition.operator == Operator::Or {
            if let (ConditionOperand::Condition(left_cond), ConditionOperand::Condition(right_cond)) =
                (&condition.left, &condition.right)
            {
                let left = self.evaluate_condition(row, left_cond);
                let right = self.evaluate_condition(row, right_cond);
                return match condition.operator {
                    Operator::And => left && right,
                    Operator::Or => left || right,
                    _ => false,
                };
            }
            return false;
        }

        // Resolve left
        let left_val = self.resolve_operand(row, &condition.left);

        match condition.operator {
            Operator::Is => left_val == Value::Null,
            Operator::IsNot => left_val != Value::Null,
            Operator::In | Operator::NotIn => {
                if left_val == Value::Null { return false; }
                let found = match &condition.right {
                    ConditionOperand::ValueList(values) => {
                        values.iter().any(|v| compare_values(&left_val, v) == 0)
                    }
                    ConditionOperand::SubQuery(sq) => {
                        if let Ok(sub_results) = self.execute(sq) {
                            sub_results.iter().any(|r| {
                                r.values().next().map_or(false, |v| compare_values(&left_val, v) == 0)
                            })
                        } else { false }
                    }
                    _ => false,
                };
                if condition.operator == Operator::NotIn { !found } else { found }
            }
            Operator::Like | Operator::NotLike => {
                if left_val == Value::Null { return false; }
                let pattern = match &condition.right {
                    ConditionOperand::Literal(s) => {
                        let s = if s.starts_with('\'') && s.ends_with('\'') && s.len() >= 2 {
                            &s[1..s.len()-1]
                        } else { s };
                        s.replace('%', ".*").replace('_', ".")
                    }
                    _ => return false,
                };
                let re = Regex::new(&format!("(?i)^{}$", pattern)).unwrap_or_else(|_| Regex::new("$^").unwrap());
                let left_str = match &left_val {
                    Value::String(s) => s.clone(),
                    Value::Number(n) => n.to_string(),
                    _ => return false,
                };
                let m = re.is_match(&left_str);
                if condition.operator == Operator::NotLike { !m } else { m }
            }
            _ => {
                // Standard comparison
                let right_val = self.resolve_operand(row, &condition.right);

                if left_val == Value::Null || right_val == Value::Null {
                    return match condition.operator {
                        Operator::Equal => left_val == Value::Null && right_val == Value::Null,
                        Operator::NotEqual => !(left_val == Value::Null && right_val == Value::Null),
                        _ => false,
                    };
                }

                let cmp = compare_values(&left_val, &right_val);
                match condition.operator {
                    Operator::Equal => cmp == 0,
                    Operator::NotEqual => cmp != 0,
                    Operator::Less => cmp < 0,
                    Operator::Greater => cmp > 0,
                    Operator::LessEq => cmp <= 0,
                    Operator::GreaterEq => cmp >= 0,
                    _ => false,
                }
            }
        }
    }

    fn resolve_operand(&self, row: &Row, operand: &ConditionOperand) -> Value {
        match operand {
            ConditionOperand::Literal(s) => {
                // Check column name
                if let Some(val) = self.resolve_value(row, s) {
                    return val;
                }
                // String literal
                if s.starts_with('\'') && s.ends_with('\'') && s.len() >= 2 {
                    return Value::String(s[1..s.len()-1].to_string());
                }
                // Number
                if let Ok(num) = s.parse::<f64>() {
                    return Value::Number(num);
                }
                Value::String(s.clone())
            }
            ConditionOperand::Null => Value::Null,
            _ => Value::Null,
        }
    }

    fn resolve_value(&self, row: &Row, path: &str) -> Option<Value> {
        if path.is_empty() { return None; }

        // Direct key match
        if let Some(val) = row.get(path) { return Some(val.clone()); }

        // Dot notation
        if path.contains('.') {
            let parts: Vec<&str> = path.split('.').collect();

            // Try progressively longer prefixes
            for i in 1..parts.len() {
                let prefix = parts[..i].join(".");
                if let Some(val) = row.get(&prefix) {
                    let mut current = val.clone();
                    for part in &parts[i..] {
                        match current {
                            Value::Object(ref map) => {
                                current = map.get(*part).cloned().unwrap_or(Value::Null);
                            }
                            _ => return None,
                        }
                    }
                    if current != Value::Null { return Some(current); }
                }
            }
        }

        None
    }
}

fn merge_rows(left: &Row, right: &Row, left_table: &str, right_table: &str) -> Row {
    let mut merged = Row::new();
    for (k, v) in left {
        merged.insert(k.clone(), v.clone());
        if !k.contains('.') {
            merged.insert(format!("{}.{}", left_table, k), v.clone());
        }
    }
    for (k, v) in right {
        merged.insert(k.clone(), v.clone());
        if !k.contains('.') {
            merged.insert(format!("{}.{}", right_table, k), v.clone());
        }
    }
    merged
}

fn parse_value_token(token: &str) -> Value {
    if token.to_uppercase() == "NULL" { return Value::Null; }
    if token.starts_with('\'') && token.ends_with('\'') && token.len() >= 2 {
        return Value::String(token[1..token.len()-1].to_string());
    }
    if let Ok(num) = token.parse::<f64>() {
        return Value::Number(num);
    }
    Value::String(token.to_string())
}

fn to_number(val: &Value) -> Option<f64> {
    match val {
        Value::Number(n) => Some(*n),
        Value::String(s) => s.parse::<f64>().ok(),
        _ => None,
    }
}

fn compare_values(left: &Value, right: &Value) -> i32 {
    if let (Some(ln), Some(rn)) = (to_number(left), to_number(right)) {
        if ln < rn { return -1; }
        else if ln > rn { return 1; }
        return 0;
    }

    let left_str = format!("{:?}", left);
    let right_str = format!("{:?}", right);
    if left_str < right_str { -1 }
    else if left_str > right_str { 1 }
    else { 0 }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_data() -> Vec<Row> {
        vec![
            Row::from([
                ("state".into(), Value::String("California".into())),
                ("region".into(), Value::String("West".into())),
                ("pop".into(), Value::Number(10000.0)),
                ("pop_male".into(), Value::Number(6000.0)),
                ("pop_female".into(), Value::Number(4000.0)),
            ]),
            Row::from([
                ("state".into(), Value::String("Texas".into())),
                ("region".into(), Value::String("South".into())),
                ("pop".into(), Value::Number(5000.0)),
                ("pop_male".into(), Value::Number(2500.0)),
                ("pop_female".into(), Value::Number(2500.0)),
            ]),
            Row::from([
                ("state".into(), Value::String("Illinois".into())),
                ("region".into(), Value::String("Midwest".into())),
                ("pop".into(), Value::Number(2000.0)),
                ("pop_male".into(), Value::Number(1200.0)),
                ("pop_female".into(), Value::Number(800.0)),
            ]),
        ]
    }

    #[test]
    fn test_select_all() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT * FROM table").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 3);
    }

    #[test]
    fn test_select_specific_columns() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT state, pop FROM table").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 3);
        assert!(!res[0].contains_key("region"));
    }

    #[test]
    fn test_where_and() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT state FROM table WHERE pop > 3000 AND region = 'West'").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 1);
    }

    #[test]
    fn test_where_or_parens() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT state FROM table WHERE pop < 3000 OR (region = 'West' AND pop > 500)").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
    }

    #[test]
    fn test_column_comparison() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT state FROM table WHERE pop_male > pop_female").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
    }

    #[test]
    fn test_null_support() {
        let mut tables = HashMap::new();
        tables.insert("users".to_string(), vec![
            Row::from([("name".into(), Value::String("Alice".into())), ("age".into(), Value::Number(30.0))]),
            Row::from([("name".into(), Value::String("Bob".into())), ("age".into(), Value::Number(25.0))]),
            Row::from([("name".into(), Value::String("Charlie".into())), ("age".into(), Value::Null)]),
        ]);
        let parser = SQLParser::new_with_tables(tables);

        let q = parser.parse("SELECT name FROM users WHERE age IS NULL").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 1);

        let q = parser.parse("SELECT name FROM users WHERE age IS NOT NULL").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
    }

    #[test]
    fn test_group_by_count() {
        let mut tables = HashMap::new();
        tables.insert("orders".to_string(), vec![
            Row::from([("product".into(), Value::String("Widget".into())), ("amount".into(), Value::Number(50.0))]),
            Row::from([("product".into(), Value::String("Gadget".into())), ("amount".into(), Value::Number(100.0))]),
            Row::from([("product".into(), Value::String("Widget".into())), ("amount".into(), Value::Number(30.0))]),
        ]);
        let parser = SQLParser::new_with_tables(tables);

        let q = parser.parse("SELECT product, COUNT(*) AS cnt FROM orders GROUP BY product").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
    }

    #[test]
    fn test_order_by() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT state, pop FROM table ORDER BY pop DESC").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res[0]["state"], Value::String("California".into()));
    }

    #[test]
    fn test_comparison_operators() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT state FROM table WHERE pop >= 5000").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);

        let q = parser.parse("SELECT state FROM table WHERE pop <= 5000").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
    }

    #[test]
    fn test_like() {
        let mut tables = HashMap::new();
        tables.insert("users".to_string(), vec![
            Row::from([("name".into(), Value::String("Alice".into()))]),
            Row::from([("name".into(), Value::String("Bob".into()))]),
        ]);
        let parser = SQLParser::new_with_tables(tables);

        let q = parser.parse("SELECT name FROM users WHERE name LIKE 'A%'").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 1);
    }

    #[test]
    fn test_in_list() {
        let mut tables = HashMap::new();
        tables.insert("users".to_string(), vec![
            Row::from([("name".into(), Value::String("Alice".into())), ("age".into(), Value::Number(30.0))]),
            Row::from([("name".into(), Value::String("Bob".into())), ("age".into(), Value::Number(25.0))]),
        ]);
        let parser = SQLParser::new_with_tables(tables);

        let q = parser.parse("SELECT name FROM users WHERE age IN (25, 30)").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
    }

    #[test]
    fn test_limit() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT * FROM table LIMIT 2").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
    }

    #[test]
    fn test_invalid_sql() {
        let parser = SQLParser::new(test_data());
        assert!(parser.parse("SELECT FROM table").is_err());
        assert!(parser.parse("SELECT state table").is_err());
        assert!(parser.parse("SELECT state FROM").is_err());
    }
}
