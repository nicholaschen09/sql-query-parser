export type Value = string | number;

export interface Row {
	[key: string]: Value;
}

export type Operator = '=' | '!=' | '<' | '>' | 'AND' | 'OR';

export interface Condition {
	left: string | Condition;
	operator: Operator;
	right: string | number | Condition;
}

export interface SelectQuery {
	type: 'SELECT';
	columns: string[];
	table: string;
	where?: Condition;
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
