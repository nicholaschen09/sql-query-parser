use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A single value in a row
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
    Object(HashMap<String, Value>),
    Array(Vec<Value>),
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
    #[serde(rename = "<=")]
    LessEq,
    #[serde(rename = ">=")]
    GreaterEq,
    #[serde(rename = "AND")]
    And,
    #[serde(rename = "OR")]
    Or,
    #[serde(rename = "LIKE")]
    Like,
    #[serde(rename = "NOT LIKE")]
    NotLike,
    #[serde(rename = "IN")]
    In,
    #[serde(rename = "NOT IN")]
    NotIn,
    #[serde(rename = "IS")]
    Is,
    #[serde(rename = "IS NOT")]
    IsNot,
}

/// A WHERE condition node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Condition {
    pub left: ConditionOperand,
    pub operator: Operator,
    pub right: ConditionOperand,
}

/// An operand in a condition
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ConditionOperand {
    Condition(Box<Condition>),
    ValueList(Vec<Value>),
    SubQuery(Box<SelectQuery>),
    Literal(String),
    Null,
}

/// A column expression (plain column or aggregate)
#[derive(Debug, Clone)]
pub struct ColumnExpr {
    pub is_aggregate: bool,
    pub name: String,
    pub func_name: String,
    pub alias: String,
}

/// A JOIN clause
#[derive(Debug, Clone)]
pub struct JoinClause {
    pub join_type: String,
    pub table: String,
    pub on: Condition,
}

/// ORDER BY clause
#[derive(Debug, Clone)]
pub struct OrderByClause {
    pub column: String,
    pub direction: String,
}

/// A parsed SELECT query (the AST)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SelectQuery {
    #[serde(rename = "type")]
    pub query_type: String,
    pub columns: Vec<String>,
    pub table: String,
    #[serde(skip)]
    pub column_exprs: Vec<ColumnExpr>,
    #[serde(skip)]
    pub joins: Vec<JoinClause>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#where: Option<Condition>,
    #[serde(skip)]
    pub group_by: Vec<String>,
    #[serde(skip)]
    pub having: Option<Condition>,
    #[serde(skip)]
    pub order_by: Vec<OrderByClause>,
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
