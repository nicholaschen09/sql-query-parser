use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A single value in a row — either a string or a number
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
}

/// A row of data: column name -> value
pub type Row = HashMap<String, Value>;

/// Comparison and logical operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Operator {
    #[serde(rename = "=")]
    Equal,
    #[serde(rename = "!=")]
    NotEqual,
    #[serde(rename = "<")]
    Less,
    #[serde(rename = ">")]
    Greater,
    #[serde(rename = "AND")]
    And,
    #[serde(rename = "OR")]
    Or,
}

/// A WHERE condition node — can be a comparison or a logical combination
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Condition {
    pub left: ConditionOperand,
    pub operator: Operator,
    pub right: ConditionOperand,
}

/// An operand in a condition: either a literal value, a column name, or a nested condition
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ConditionOperand {
    Condition(Box<Condition>),
    Literal(String),
}

/// A parsed SELECT query (the AST)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SelectQuery {
    #[serde(rename = "type")]
    pub query_type: String,
    pub columns: Vec<String>,
    pub table: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#where: Option<Condition>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub limit: Option<usize>,
}

/// The result of executing a query
#[derive(Debug, Serialize, Deserialize)]
pub struct QueryResult {
    pub success: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Vec<Row>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}
