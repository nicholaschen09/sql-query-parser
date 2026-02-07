pub mod types;

use types::*;

pub struct SQLParser {
    data: Vec<Row>,
}

impl SQLParser {
    pub fn new(data: Vec<Row>) -> Self {
        SQLParser { data }
    }

    /// Parse a SQL query string into a SelectQuery AST
    pub fn parse(&self, query: &str) -> Result<SelectQuery, String> {
        let tokens = self.tokenize(query);
        self.parse_select(&tokens)
    }

    /// Execute a parsed query against the data
    pub fn execute(&self, query: &SelectQuery) -> Result<Vec<Row>, String> {
        let mut results = self.data.clone();

        // Apply WHERE clause
        if let Some(ref condition) = query.r#where {
            results = results
                .into_iter()
                .filter(|row| self.evaluate_condition(row, condition))
                .collect();
        }

        // Apply column selection
        if !query.columns.is_empty() && query.columns[0] != "*" {
            results = results
                .into_iter()
                .map(|row| {
                    let mut new_row = Row::new();
                    for col in &query.columns {
                        if let Some(val) = row.get(col) {
                            new_row.insert(col.clone(), val.clone());
                        }
                    }
                    new_row
                })
                .collect();
        }

        // Apply LIMIT
        if let Some(limit) = query.limit {
            results.truncate(limit);
        }

        Ok(results)
    }

    fn tokenize(&self, query: &str) -> Vec<String> {
        let query = query
            .replace('(', " ( ")
            .replace(')', " ) ")
            .replace(',', " , ");
        let query = query.trim_end_matches(';');

        query
            .split_whitespace()
            .map(|s| s.to_string())
            .collect()
    }

    fn parse_select(&self, tokens: &[String]) -> Result<SelectQuery, String> {
        if tokens.is_empty() || tokens[0].to_uppercase() != "SELECT" {
            return Err("query must start with SELECT".to_string());
        }

        // Find FROM index
        let from_index = tokens
            .iter()
            .position(|t| t.to_uppercase() == "FROM")
            .ok_or("expected FROM clause")?;

        // Parse columns
        if from_index <= 1 {
            return Err("no columns specified in SELECT".to_string());
        }
        let columns = self.parse_columns(&tokens[1..from_index]);
        if columns.is_empty() || (columns.len() == 1 && columns[0].is_empty()) {
            return Err("no columns specified in SELECT".to_string());
        }

        // Parse table name
        if from_index + 1 >= tokens.len() {
            return Err("expected table name after FROM".to_string());
        }
        let table = tokens[from_index + 1].clone();

        let mut current_index = from_index + 2;
        let mut where_clause = None;
        let mut limit = None;

        // Parse WHERE clause
        if current_index < tokens.len() && tokens[current_index].to_uppercase() == "WHERE" {
            current_index += 1;
            let (cond, next_idx) = self.parse_condition_recursive(tokens, current_index)?;
            where_clause = Some(cond);
            current_index = next_idx;
        }

        // Parse LIMIT clause
        if current_index < tokens.len() && tokens[current_index].to_uppercase() == "LIMIT" {
            current_index += 1;
            if current_index >= tokens.len() {
                return Err("expected number after LIMIT".to_string());
            }
            let n: usize = tokens[current_index]
                .parse()
                .map_err(|_| "LIMIT must be a non-negative integer".to_string())?;
            limit = Some(n);
        }

        Ok(SelectQuery {
            query_type: "SELECT".to_string(),
            columns,
            table,
            r#where: where_clause,
            limit,
        })
    }

    fn parse_columns(&self, tokens: &[String]) -> Vec<String> {
        tokens
            .iter()
            .filter(|t| *t != "," && !t.is_empty())
            .map(|t| t.trim_matches(',').to_string())
            .filter(|t| !t.is_empty())
            .collect()
    }

    /// Parse OR (lowest precedence)
    fn parse_condition_recursive(
        &self,
        tokens: &[String],
        idx: usize,
    ) -> Result<(Condition, usize), String> {
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

    /// Parse AND (higher precedence)
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

    /// Parse a single comparison or parenthesized expression
    fn parse_operand(&self, tokens: &[String], idx: usize) -> Result<(Condition, usize), String> {
        if idx >= tokens.len() {
            return Err("unexpected end of tokens".to_string());
        }

        // Handle parentheses
        if tokens[idx] == "(" {
            let (cond, next_idx) = self.parse_condition_recursive(tokens, idx + 1)?;
            if next_idx >= tokens.len() || tokens[next_idx] != ")" {
                return Err("expected closing parenthesis".to_string());
            }
            return Ok((cond, next_idx + 1));
        }

        // Parse comparison: left op right
        if idx + 2 >= tokens.len() {
            return Err("incomplete condition".to_string());
        }

        let left = tokens[idx].clone();
        let operator_str = &tokens[idx + 1];
        let right_token = tokens[idx + 2].clone();

        let valid_operators = ["=", "!=", "<", ">", "AND", "OR"];

        if !valid_operators.contains(&operator_str.as_str()) {
            return Err(format!(
                "missing or invalid operator in condition near '{}'",
                left
            ));
        }

        if valid_operators.contains(&right_token.as_str()) {
            return Err(format!(
                "unexpected operator '{}' after operator '{}'",
                right_token, operator_str
            ));
        }

        let operator = match operator_str.as_str() {
            "=" => Operator::Equal,
            "!=" => Operator::NotEqual,
            "<" => Operator::Less,
            ">" => Operator::Greater,
            _ => return Err(format!("invalid operator: {}", operator_str)),
        };

        Ok((
            Condition {
                left: ConditionOperand::Literal(left),
                operator,
                right: ConditionOperand::Literal(right_token),
            },
            idx + 3,
        ))
    }

    fn evaluate_condition(&self, row: &Row, condition: &Condition) -> bool {
        // Handle logical operators
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

        // Handle comparison operators
        let left_val = self.resolve_operand(row, &condition.left);
        let right_val = self.resolve_operand(row, &condition.right);

        match condition.operator {
            Operator::Equal => compare_values(&left_val, &right_val) == 0,
            Operator::NotEqual => compare_values(&left_val, &right_val) != 0,
            Operator::Less => compare_values(&left_val, &right_val) < 0,
            Operator::Greater => compare_values(&left_val, &right_val) > 0,
            _ => false,
        }
    }

    fn resolve_operand(&self, row: &Row, operand: &ConditionOperand) -> Value {
        match operand {
            ConditionOperand::Literal(s) => {
                // Check if it's a column name
                if let Some(val) = row.get(s) {
                    return val.clone();
                }
                // Check if it's a string literal (quoted)
                if s.starts_with('\'') && s.ends_with('\'') && s.len() >= 2 {
                    return Value::String(s[1..s.len() - 1].to_string());
                }
                // Check if it's a number
                if let Ok(num) = s.parse::<f64>() {
                    return Value::Number(num);
                }
                Value::String(s.clone())
            }
            ConditionOperand::Condition(_) => Value::Null,
        }
    }
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
        if ln < rn {
            return -1;
        } else if ln > rn {
            return 1;
        }
        return 0;
    }

    let left_str = format!("{:?}", left);
    let right_str = format!("{:?}", right);
    if left_str < right_str {
        -1
    } else if left_str > right_str {
        1
    } else {
        0
    }
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
        assert_eq!(res[0]["state"], Value::String("California".into()));
    }

    #[test]
    fn test_select_specific_columns() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT state, pop FROM table").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 3);
        assert_eq!(res[0]["state"], Value::String("California".into()));
        assert_eq!(res[0]["pop"], Value::Number(10000.0));
        assert!(!res[0].contains_key("region"));
    }

    #[test]
    fn test_where_and() {
        let parser = SQLParser::new(test_data());
        let q = parser
            .parse("SELECT state FROM table WHERE pop > 3000 AND region = 'West'")
            .unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(res[0]["state"], Value::String("California".into()));
    }

    #[test]
    fn test_where_or_with_parentheses() {
        let parser = SQLParser::new(test_data());
        let q = parser
            .parse("SELECT state FROM table WHERE pop < 3000 OR (region = 'West' AND pop > 500)")
            .unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
        let states: Vec<&Value> = res.iter().map(|r| &r["state"]).collect();
        assert!(states.contains(&&Value::String("California".into())));
        assert!(states.contains(&&Value::String("Illinois".into())));
    }

    #[test]
    fn test_column_to_column_comparison() {
        let parser = SQLParser::new(test_data());
        let q = parser
            .parse("SELECT state FROM table WHERE pop_male > pop_female")
            .unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
        let states: Vec<&Value> = res.iter().map(|r| &r["state"]).collect();
        assert!(states.contains(&&Value::String("California".into())));
        assert!(states.contains(&&Value::String("Illinois".into())));
    }

    #[test]
    fn test_no_matches() {
        let parser = SQLParser::new(test_data());
        let q = parser
            .parse("SELECT state FROM table WHERE pop > 99999")
            .unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 0);
    }

    #[test]
    fn test_invalid_sql() {
        let parser = SQLParser::new(test_data());
        assert!(parser.parse("SELECT FROM table").is_err());
        assert!(parser.parse("SELECT state table").is_err());
        assert!(parser.parse("SELECT state FROM").is_err());
    }

    #[test]
    fn test_limit() {
        let parser = SQLParser::new(test_data());
        let q = parser.parse("SELECT * FROM table LIMIT 2").unwrap();
        let res = parser.execute(&q).unwrap();
        assert_eq!(res.len(), 2);
    }
}
