type Value = string | number;

interface Row {
    [key: string]: Value;
}

type Operator = '=' | '!=' | '<' | '>' | 'AND' | 'OR';

interface Condition {
    left: string | Condition;
    operator: Operator;
    right: string | number | Condition;
}

interface SelectQuery {
    type: 'SELECT';
    columns: string[];
    table: string;
    where?: Condition;
    limit?: number;
}

interface QueryResult {
    success: boolean;
    data?: Row[];
    error?: string;
}

interface QueryHistory {
    id: number;
    query: string;
    result: QueryResult;
    timestamp: string;
}

module.exports = {
    Value,
    Row,
    Operator,
    Condition,
    SelectQuery,
    QueryResult,
    QueryHistory
}; 