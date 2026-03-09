import type { Condition, Row, SelectQuery } from '../types/types';
import { evaluateCondition } from './condition-evaluator';
import { executeSelectQuery } from './executor';
import { parseSelectFromTokens } from './select-parser';
import { parseValueToken, tokenize } from './tokenizer';
import { resolveValue } from './value-resolver';

export class SQLParser {
	private tables: Record<string, Row[]>;

	constructor(data: Row[] | Record<string, Row[]>) {
		if (Array.isArray(data)) {
			this.tables = { table: data };
		} else {
			this.tables = data;
		}
	}

	parse(query: string): SelectQuery {
		const tokens = tokenize(query);
		return parseSelectFromTokens(tokens, parseValueToken);
	}

	execute(query: SelectQuery): Row[] {
		return executeSelectQuery(query, {
			getTableData: (name: string) => this.getTableData(name),
			resolveValue,
			evaluateCondition: (row: Row, condition: Condition) =>
				evaluateCondition(row, condition, {
					resolveValue,
					executeSubquery: (subQuery: SelectQuery) => this.execute(subQuery)
				})
		});
	}

	private getTableData(name: string): Row[] | undefined {
		if (this.tables[name]) return this.tables[name];
		const lower = name.toLowerCase();
		for (const [key, val] of Object.entries(this.tables)) {
			if (key.toLowerCase() === lower) return val;
		}
		if (this.tables['table']) return this.tables['table'];
		const keys = Object.keys(this.tables);
		if (keys.length === 1) return this.tables[keys[0]];
		return undefined;
	}
}
