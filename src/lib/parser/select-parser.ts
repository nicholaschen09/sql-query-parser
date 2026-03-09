import type {
	SelectQuery,
	Condition,
	Operator,
	ColumnExpr,
	AggregateColumn,
	AggregateFunc,
	JoinClause,
	OrderByClause
} from '../types/types';

class SelectParserEngine {
	constructor(private readonly parseValueToken: (token: string) => any) {}

	parseSelect(tokens: string[]): SelectQuery {
		if (!tokens.length || tokens[0].toUpperCase() !== 'SELECT') {
			throw new Error('Query must start with SELECT');
		}

		// Find FROM index, skipping parenthesized expressions (for aggregates)
		let depth = 0;
		let fromIndex = -1;
		for (let i = 1; i < tokens.length; i++) {
			if (tokens[i] === '(') depth++;
			else if (tokens[i] === ')') depth--;
			else if (depth === 0 && tokens[i].toUpperCase() === 'FROM') {
				fromIndex = i;
				break;
			}
		}
		if (fromIndex === -1) throw new Error('Expected FROM clause');

		const columns = this.parseColumnExprs(tokens, 1, fromIndex);
		if (columns.length === 0 || (columns.length === 1 && columns[0] === '')) {
			throw new Error('No columns specified in SELECT');
		}

		if (fromIndex + 1 >= tokens.length) throw new Error('Expected table name after FROM');
		const table = tokens[fromIndex + 1];
		let idx = fromIndex + 2;

		// Parse JOINs
		let joins: JoinClause[] | undefined;
		const [joinList, afterJoins] = this.parseJoins(tokens, idx);
		if (joinList.length > 0) joins = joinList;
		idx = afterJoins;

		// Parse WHERE
		let where: Condition | undefined;
		if (idx < tokens.length && tokens[idx].toUpperCase() === 'WHERE') {
			idx++;
			const [cond, nextIdx] = this.parseConditionRecursive(tokens, idx);
			where = cond;
			idx = nextIdx;
		}

		// Parse GROUP BY
		let groupBy: string[] | undefined;
		if (
			idx < tokens.length &&
			tokens[idx].toUpperCase() === 'GROUP' &&
			idx + 1 < tokens.length &&
			tokens[idx + 1].toUpperCase() === 'BY'
		) {
			idx += 2;
			groupBy = [];
			while (idx < tokens.length) {
				const upper = tokens[idx].toUpperCase();
				if (upper === 'HAVING' || upper === 'ORDER' || upper === 'LIMIT') break;
				if (tokens[idx] !== ',') groupBy.push(tokens[idx]);
				idx++;
			}
		}

		// Parse HAVING
		let having: Condition | undefined;
		if (idx < tokens.length && tokens[idx].toUpperCase() === 'HAVING') {
			idx++;
			const [cond, nextIdx] = this.parseConditionRecursive(tokens, idx);
			having = cond;
			idx = nextIdx;
		}

		// Parse ORDER BY
		let orderBy: OrderByClause[] | undefined;
		if (
			idx < tokens.length &&
			tokens[idx].toUpperCase() === 'ORDER' &&
			idx + 1 < tokens.length &&
			tokens[idx + 1].toUpperCase() === 'BY'
		) {
			idx += 2;
			orderBy = [];
			while (idx < tokens.length) {
				if (tokens[idx].toUpperCase() === 'LIMIT') break;
				if (tokens[idx] === ',') {
					idx++;
					continue;
				}
				const col = tokens[idx];
				idx++;
				let direction: 'ASC' | 'DESC' = 'ASC';
				if (idx < tokens.length) {
					const dir = tokens[idx].toUpperCase();
					if (dir === 'ASC' || dir === 'DESC') {
						direction = dir as 'ASC' | 'DESC';
						idx++;
					}
				}
				orderBy.push({ column: col, direction });
			}
		}

		// Parse LIMIT
		let limit: number | undefined;
		if (idx < tokens.length && tokens[idx].toUpperCase() === 'LIMIT') {
			idx++;
			if (idx >= tokens.length) throw new Error('Expected number after LIMIT');
			limit = parseInt(tokens[idx]);
			if (isNaN(limit) || limit < 0) throw new Error('LIMIT must be a non-negative integer');
		}

		return { type: 'SELECT', columns, table, joins, where, groupBy, having, orderBy, limit };
	}

	private parseColumnExprs(tokens: string[], start: number, end: number): ColumnExpr[] {
		const columns: ColumnExpr[] = [];
		let i = start;
		const aggFuncs = ['COUNT', 'SUM', 'AVG', 'MIN', 'MAX'];

		while (i < end) {
			if (tokens[i] === ',' || tokens[i] === '') {
				i++;
				continue;
			}

			const upper = tokens[i].toUpperCase();
			// Check for aggregate functions: FUNC ( col )
			if (aggFuncs.includes(upper) && i + 1 < end && tokens[i + 1] === '(') {
				const func = upper as AggregateFunc;
				let j = i + 2;
				let col = '';
				while (j < end && tokens[j] !== ')') {
					if (col) col += ' ';
					col += tokens[j];
					j++;
				}
				j++; // skip )

				let alias: string | undefined;
				if (j < end && tokens[j].toUpperCase() === 'AS' && j + 1 < end) {
					alias = tokens[j + 1];
					j += 2;
				}
				columns.push({ func, column: col || '*', alias } as AggregateColumn);
				i = j;
			} else {
				const colName = tokens[i].replace(/,/g, '');
				if (colName) columns.push(colName);
				i++;
			}
		}
		return columns;
	}

	private parseJoins(tokens: string[], idx: number): [JoinClause[], number] {
		const joins: JoinClause[] = [];

		while (idx < tokens.length) {
			const upper = tokens[idx].toUpperCase();
			let joinType: 'INNER' | 'LEFT' | 'RIGHT' | null = null;

			if (upper === 'JOIN') {
				joinType = 'INNER';
				idx++;
			} else if (
				upper === 'INNER' &&
				idx + 1 < tokens.length &&
				tokens[idx + 1].toUpperCase() === 'JOIN'
			) {
				joinType = 'INNER';
				idx += 2;
			} else if (upper === 'LEFT') {
				joinType = 'LEFT';
				idx++;
				if (idx < tokens.length && tokens[idx].toUpperCase() === 'JOIN') idx++;
			} else if (upper === 'RIGHT') {
				joinType = 'RIGHT';
				idx++;
				if (idx < tokens.length && tokens[idx].toUpperCase() === 'JOIN') idx++;
			} else {
				break;
			}

			if (idx >= tokens.length) throw new Error('Expected table name after JOIN');
			const table = tokens[idx];
			idx++;

			if (idx >= tokens.length || tokens[idx].toUpperCase() !== 'ON') {
				throw new Error('Expected ON after JOIN table name');
			}
			idx++;

			const [cond, nextIdx] = this.parseConditionRecursive(tokens, idx);
			joins.push({ joinType, table, on: cond });
			idx = nextIdx;
		}

		return [joins, idx];
	}

	// Parse OR (lowest precedence)
	private parseConditionRecursive(tokens: string[], idx: number): [Condition, number] {
		let [left, nextIdx] = this.parseAnd(tokens, idx);
		while (nextIdx < tokens.length && tokens[nextIdx]?.toUpperCase() === 'OR') {
			const [right, afterRight] = this.parseAnd(tokens, nextIdx + 1);
			left = { left, operator: 'OR' as Operator, right };
			nextIdx = afterRight;
		}
		return [left, nextIdx];
	}

	// Parse AND (higher precedence)
	private parseAnd(tokens: string[], idx: number): [Condition, number] {
		let [left, nextIdx] = this.parseOperand(tokens, idx);
		while (nextIdx < tokens.length && tokens[nextIdx]?.toUpperCase() === 'AND') {
			const [right, afterRight] = this.parseOperand(tokens, nextIdx + 1);
			left = { left, operator: 'AND' as Operator, right };
			nextIdx = afterRight;
		}
		return [left, nextIdx];
	}

	// Parse a single condition operand
	private parseOperand(tokens: string[], idx: number): [Condition, number] {
		if (idx >= tokens.length) throw new Error('Unexpected end of condition');

		// Parenthesized expression
		if (tokens[idx] === '(') {
			const [cond, nextIdx] = this.parseConditionRecursive(tokens, idx + 1);
			if (nextIdx >= tokens.length || tokens[nextIdx] !== ')') {
				throw new Error('Expected closing parenthesis');
			}
			return [cond, nextIdx + 1];
		}

		const left = tokens[idx];
		let opIdx = idx + 1;

		if (opIdx >= tokens.length) {
			throw new Error(`Incomplete condition near '${left}'`);
		}

		// IS NULL / IS NOT NULL
		if (tokens[opIdx]?.toUpperCase() === 'IS') {
			if (
				opIdx + 1 < tokens.length &&
				tokens[opIdx + 1].toUpperCase() === 'NOT' &&
				opIdx + 2 < tokens.length &&
				tokens[opIdx + 2].toUpperCase() === 'NULL'
			) {
				return [{ left, operator: 'IS NOT', right: null }, opIdx + 3];
			}
			if (opIdx + 1 < tokens.length && tokens[opIdx + 1].toUpperCase() === 'NULL') {
				return [{ left, operator: 'IS', right: null }, opIdx + 2];
			}
		}

		// NOT IN / NOT LIKE
		let hasNot = false;
		if (tokens[opIdx]?.toUpperCase() === 'NOT') {
			hasNot = true;
			opIdx++;
		}

		// IN (values) / IN (SELECT ...)
		if (opIdx < tokens.length && tokens[opIdx]?.toUpperCase() === 'IN') {
			opIdx++;
			if (opIdx >= tokens.length || tokens[opIdx] !== '(') {
				throw new Error('Expected ( after IN');
			}
			opIdx++;

			// Subquery?
			if (opIdx < tokens.length && tokens[opIdx]?.toUpperCase() === 'SELECT') {
				const subTokens: string[] = [];
				let subDepth = 1;
				let j = opIdx;
				while (j < tokens.length && subDepth > 0) {
					if (tokens[j] === '(') subDepth++;
					if (tokens[j] === ')') {
						subDepth--;
						if (subDepth === 0) break;
					}
					subTokens.push(tokens[j]);
					j++;
				}
				const subQuery = this.parseSelect(subTokens);
				return [
					{ left, operator: (hasNot ? 'NOT IN' : 'IN') as Operator, right: subQuery },
					j + 1
				];
			}

			// Value list
			const values: any[] = [];
			while (opIdx < tokens.length && tokens[opIdx] !== ')') {
				if (tokens[opIdx] !== ',') values.push(this.parseValueToken(tokens[opIdx]));
				opIdx++;
			}
			opIdx++; // skip )
			return [
				{ left, operator: (hasNot ? 'NOT IN' : 'IN') as Operator, right: values },
				opIdx
			];
		}

		// LIKE / NOT LIKE
		if (opIdx < tokens.length && tokens[opIdx]?.toUpperCase() === 'LIKE') {
			opIdx++;
			if (opIdx >= tokens.length) throw new Error('Expected pattern after LIKE');
			const right = this.parseValueToken(tokens[opIdx]);
			return [
				{ left, operator: (hasNot ? 'NOT LIKE' : 'LIKE') as Operator, right },
				opIdx + 1
			];
		}

		// If we consumed NOT but didn't find IN or LIKE, put it back
		if (hasNot) opIdx--;

		// Standard comparison: =, !=, <, >, <=, >=
		const operator = tokens[opIdx];
		const validOps = ['=', '!=', '<', '>', '<=', '>='];
		if (!validOps.includes(operator)) {
			throw new Error(`Missing or invalid operator in condition near '${left}'`);
		}
		if (opIdx + 1 >= tokens.length) {
			throw new Error(`Missing right-hand side in condition near '${left} ${operator}'`);
		}

		const rightToken = tokens[opIdx + 1];
		if (validOps.includes(rightToken) || ['AND', 'OR'].includes(rightToken.toUpperCase())) {
			throw new Error(`Unexpected operator '${rightToken}' after operator '${operator}'`);
		}

		const right = this.parseValueToken(rightToken);
		return [{ left, operator: operator as Operator, right }, opIdx + 2];
	}
}

export function parseSelectFromTokens(
	tokens: string[],
	parseValueToken: (token: string) => any
): SelectQuery {
	const engine = new SelectParserEngine(parseValueToken);
	return engine.parseSelect(tokens);
}
