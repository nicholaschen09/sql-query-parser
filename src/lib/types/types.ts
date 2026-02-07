export type Value = string | number | boolean | null;

export interface Row {
	[key: string]: any;
}

export type Operator =
	| '='
	| '!='
	| '<'
	| '>'
	| '<='
	| '>='
	| 'AND'
	| 'OR'
	| 'LIKE'
	| 'NOT LIKE'
	| 'IN'
	| 'NOT IN'
	| 'IS'
	| 'IS NOT';

export type AggregateFunc = 'COUNT' | 'SUM' | 'AVG' | 'MIN' | 'MAX';

export interface AggregateColumn {
	func: AggregateFunc;
	column: string;
	alias?: string;
}

export type ColumnExpr = string | AggregateColumn;

export interface JoinClause {
	joinType: 'INNER' | 'LEFT' | 'RIGHT';
	table: string;
	on: Condition;
}

export interface OrderByClause {
	column: string;
	direction: 'ASC' | 'DESC';
}

export interface Condition {
	left: string | Condition;
	operator: Operator;
	right: any;
}

export interface SelectQuery {
	type: 'SELECT';
	columns: ColumnExpr[];
	table: string;
	joins?: JoinClause[];
	where?: Condition;
	groupBy?: string[];
	having?: Condition;
	orderBy?: OrderByClause[];
	limit?: number;
}

export interface QueryResult {
	success: boolean;
	data?: Row[];
	error?: string;
}

export interface QueryHistory {
	id: number;
	query: string;
	result: QueryResult;
	timestamp: string;
}
