import type {
	Row,
	SelectQuery,
	Condition,
	Operator,
	ColumnExpr,
	AggregateColumn,
	AggregateFunc,
	JoinClause,
	OrderByClause
} from '../types/types';

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
		const tokens = this.tokenize(query);
		return this.parseSelect(tokens);
	}

	private tokenize(query: string): string[] {
		const tokens: string[] = [];
		let i = 0;
		const q = query.trim().replace(/;$/, '');

		while (i < q.length) {
			// Skip whitespace
			if (/\s/.test(q[i])) {
				i++;
				continue;
			}

			// String literals (single-quoted, supports spaces inside)
			if (q[i] === "'") {
				let j = i + 1;
				while (j < q.length && q[j] !== "'") j++;
				tokens.push(q.substring(i, j + 1));
				i = j + 1;
				continue;
			}

			// Two-char operators: >=, <=, !=
			if (i + 1 < q.length) {
				const two = q.substring(i, i + 2);
				if (two === '>=' || two === '<=' || two === '!=') {
					tokens.push(two);
					i += 2;
					continue;
				}
			}

			// Single-char punctuation
			if ('(),=<>*'.includes(q[i])) {
				tokens.push(q[i]);
				i++;
				continue;
			}

			// Words: identifiers, keywords, numbers, dotted names
			let j = i;
			while (j < q.length && !/[\s(),=<>!*']/.test(q[j])) {
				if (q[j] === '!' && j + 1 < q.length && q[j + 1] === '=') break;
				j++;
			}
			if (j > i) {
				tokens.push(q.substring(i, j));
			}
			i = Math.max(i + 1, j);
		}
		return tokens;
	}

	private parseSelect(tokens: string[]): SelectQuery {
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

	private parseValueToken(token: string): any {
		if (token.toUpperCase() === 'NULL') return null;
		if (token.startsWith("'") && token.endsWith("'") && token.length >= 2) {
			return token.slice(1, -1);
		}
		const num = Number(token);
		if (!isNaN(num) && token !== '') return num;
		return token;
	}

	execute(query: SelectQuery): Row[] {
		// 1. Get base table data
		let results = this.getTableData(query.table);
		if (!results) throw new Error(`Table '${query.table}' not found`);
		results = results.map((r) => ({ ...r }));

		// 2. Apply JOINs
		if (query.joins && query.joins.length > 0) {
			results = this.executeJoins(results, query.table, query.joins);
		}

		// 3. Apply WHERE
		if (query.where) {
			results = results.filter((row) => this.evaluateCondition(row, query.where!));
		}

		// 4. GROUP BY + aggregations
		const hasAgg = this.hasAggregates(query.columns);
		if (query.groupBy || hasAgg) {
			results = this.executeGroupBy(
				results,
				query.groupBy || [],
				query.columns,
				query.having
			);
		} else {
			// 5. Column projection (no GROUP BY)
			if (
				query.columns.length > 0 &&
				!(query.columns.length === 1 && query.columns[0] === '*')
			) {
				results = results.map((row) => {
					const newRow: Row = {};
					for (const col of query.columns) {
						if (typeof col === 'string') {
							const val = this.resolveValue(row, col);
							if (val !== undefined) {
								newRow[col] = val;
							}
						}
					}
					return newRow;
				});
			}
		}

		// 6. Apply HAVING (if no GROUP BY but has aggregates, already handled)

		// 7. Apply ORDER BY
		if (query.orderBy && query.orderBy.length > 0) {
			results = this.executeOrderBy(results, query.orderBy);
		}

		// 8. Apply LIMIT
		if (query.limit !== undefined) {
			results = results.slice(0, query.limit);
		}

		return results;
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

	private executeJoins(leftRows: Row[], leftTable: string, joins: JoinClause[]): Row[] {
		let results = leftRows;

		for (const join of joins) {
			const rightData = this.getTableData(join.table);
			if (!rightData) throw new Error(`Table '${join.table}' not found for JOIN`);

			const joined: Row[] = [];

			if (join.joinType === 'RIGHT') {
				for (const rightRow of rightData) {
					let matched = false;
					for (const leftRow of results) {
						const merged = this.mergeRows(leftRow, rightRow, leftTable, join.table);
						if (this.evaluateCondition(merged, join.on)) {
							joined.push(merged);
							matched = true;
						}
					}
					if (!matched) {
						const merged: Row = {};
						if (results.length > 0) {
							for (const key of Object.keys(results[0])) {
								if (!key.includes('.')) {
									merged[key] = null;
									merged[`${leftTable}.${key}`] = null;
								}
							}
						}
						for (const [k, v] of Object.entries(rightRow)) {
							merged[k] = v;
							merged[`${join.table}.${k}`] = v;
						}
						joined.push(merged);
					}
				}
			} else {
				// INNER or LEFT
				for (const leftRow of results) {
					let matched = false;
					for (const rightRow of rightData) {
						const merged = this.mergeRows(leftRow, rightRow, leftTable, join.table);
						if (this.evaluateCondition(merged, join.on)) {
							joined.push(merged);
							matched = true;
						}
					}
					if (!matched && join.joinType === 'LEFT') {
						const merged: Row = {};
						for (const [k, v] of Object.entries(leftRow)) {
							merged[k] = v;
							if (!k.includes('.')) merged[`${leftTable}.${k}`] = v;
						}
						if (rightData.length > 0) {
							for (const key of Object.keys(rightData[0])) {
								merged[key] = null;
								merged[`${join.table}.${key}`] = null;
							}
						}
						joined.push(merged);
					}
				}
			}

			results = joined;
		}

		return results;
	}

	private mergeRows(leftRow: Row, rightRow: Row, leftTable: string, rightTable: string): Row {
		const merged: Row = {};
		for (const [k, v] of Object.entries(leftRow)) {
			merged[k] = v;
			if (!k.includes('.')) merged[`${leftTable}.${k}`] = v;
		}
		for (const [k, v] of Object.entries(rightRow)) {
			merged[k] = v;
			if (!k.includes('.')) merged[`${rightTable}.${k}`] = v;
		}
		return merged;
	}

	private executeGroupBy(
		rows: Row[],
		groupBy: string[],
		columns: ColumnExpr[],
		having?: Condition
	): Row[] {
		const groups = new Map<string, Row[]>();

		if (groupBy.length === 0) {
			groups.set('__all__', rows);
		} else {
			for (const row of rows) {
				const key = groupBy
					.map((col) => JSON.stringify(this.resolveValue(row, col) ?? null))
					.join('|');
				if (!groups.has(key)) groups.set(key, []);
				groups.get(key)!.push(row);
			}
		}

		let results: Row[] = [];
		for (const [, groupRows] of groups) {
			const newRow: Row = {};

			for (const col of groupBy) {
				newRow[col] = this.resolveValue(groupRows[0], col);
			}

			for (const col of columns) {
				if (typeof col === 'object' && 'func' in col) {
					const agg = col as AggregateColumn;
					const alias = agg.alias || `${agg.func}(${agg.column})`;
					newRow[alias] = this.computeAggregate(agg.func, agg.column, groupRows);
				} else if (typeof col === 'string' && col !== '*' && !groupBy.includes(col)) {
					newRow[col] = this.resolveValue(groupRows[0], col);
				}
			}

			results.push(newRow);
		}

		if (having) {
			results = results.filter((row) => this.evaluateCondition(row, having));
		}

		return results;
	}

	private computeAggregate(func: string, column: string, rows: Row[]): any {
		switch (func) {
			case 'COUNT':
				if (column === '*') return rows.length;
				return rows.filter((r) => this.resolveValue(r, column) != null).length;
			case 'SUM': {
				let sum = 0;
				for (const r of rows) {
					const v = this.resolveValue(r, column);
					if (typeof v === 'number') sum += v;
				}
				return sum;
			}
			case 'AVG': {
				let sum = 0,
					count = 0;
				for (const r of rows) {
					const v = this.resolveValue(r, column);
					if (typeof v === 'number') {
						sum += v;
						count++;
					}
				}
				return count > 0 ? sum / count : null;
			}
			case 'MIN': {
				let min: any = null;
				for (const r of rows) {
					const v = this.resolveValue(r, column);
					if (v != null && (min === null || v < min)) min = v;
				}
				return min;
			}
			case 'MAX': {
				let max: any = null;
				for (const r of rows) {
					const v = this.resolveValue(r, column);
					if (v != null && (max === null || v > max)) max = v;
				}
				return max;
			}
			default:
				return null;
		}
	}

	private hasAggregates(columns: ColumnExpr[]): boolean {
		return columns.some((c) => typeof c === 'object' && 'func' in c);
	}

	private executeOrderBy(rows: Row[], orderBy: OrderByClause[]): Row[] {
		return [...rows].sort((a, b) => {
			for (const ob of orderBy) {
				const av = this.resolveValue(a, ob.column);
				const bv = this.resolveValue(b, ob.column);
				let cmp = 0;
				if (av == null && bv == null) cmp = 0;
				else if (av == null) cmp = -1;
				else if (bv == null) cmp = 1;
				else if (typeof av === 'number' && typeof bv === 'number') cmp = av - bv;
				else cmp = String(av).localeCompare(String(bv));
				if (ob.direction === 'DESC') cmp = -cmp;
				if (cmp !== 0) return cmp;
			}
			return 0;
		});
	}

	private evaluateCondition(row: Row, condition: Condition): boolean {
		// Logical operators
		if (condition.operator === 'AND' || condition.operator === 'OR') {
			const left = this.evaluateCondition(row, condition.left as Condition);
			const right = this.evaluateCondition(row, condition.right as Condition);
			return condition.operator === 'AND' ? left && right : left || right;
		}

		// Resolve left value
		let leftVal: any;
		if (typeof condition.left === 'string') {
			const resolved = this.resolveValue(row, condition.left);
			leftVal = resolved !== undefined ? resolved : condition.left;
		} else {
			leftVal = this.evaluateCondition(row, condition.left);
		}

		switch (condition.operator) {
			case 'IS':
				return leftVal === null || leftVal === undefined;
			case 'IS NOT':
				return leftVal !== null && leftVal !== undefined;
			case 'IN': {
				if (leftVal === null || leftVal === undefined) return false;
				if (Array.isArray(condition.right)) {
					return condition.right.some((v: any) => v === leftVal);
				}
				if (condition.right?.type === 'SELECT') {
					const subResults = this.execute(condition.right as SelectQuery);
					return subResults.some((r) => Object.values(r)[0] === leftVal);
				}
				return false;
			}
			case 'NOT IN': {
				if (leftVal === null || leftVal === undefined) return false;
				if (Array.isArray(condition.right)) {
					return !condition.right.some((v: any) => v === leftVal);
				}
				if (condition.right?.type === 'SELECT') {
					const subResults = this.execute(condition.right as SelectQuery);
					return !subResults.some((r) => Object.values(r)[0] === leftVal);
				}
				return true;
			}
			case 'LIKE': {
				if (leftVal == null) return false;
				const pattern = String(condition.right)
					.replace(/%/g, '.*')
					.replace(/_/g, '.');
				return new RegExp(`^${pattern}$`, 'i').test(String(leftVal));
			}
			case 'NOT LIKE': {
				if (leftVal == null) return false;
				const pattern = String(condition.right)
					.replace(/%/g, '.*')
					.replace(/_/g, '.');
				return !new RegExp(`^${pattern}$`, 'i').test(String(leftVal));
			}
			default: {
				// Standard comparison: =, !=, <, >, <=, >=
				let rightVal: any;
				if (typeof condition.right === 'string') {
					const resolved = this.resolveValue(row, condition.right);
					rightVal = resolved !== undefined ? resolved : condition.right;
				} else {
					rightVal = condition.right;
				}

				// Handle null comparisons
				if (leftVal == null || rightVal == null) {
					if (condition.operator === '=') return leftVal === rightVal;
					if (condition.operator === '!=') return leftVal !== rightVal;
					return false;
				}

				// Numeric coercion
				const ln = typeof leftVal === 'number' ? leftVal : Number(leftVal);
				const rn = typeof rightVal === 'number' ? rightVal : Number(rightVal);
				const useNumeric =
					!isNaN(ln) && !isNaN(rn) && leftVal !== '' && rightVal !== '';

				switch (condition.operator) {
					case '=':
						return useNumeric ? ln === rn : leftVal === rightVal;
					case '!=':
						return useNumeric ? ln !== rn : leftVal !== rightVal;
					case '<':
						return useNumeric ? ln < rn : leftVal < rightVal;
					case '>':
						return useNumeric ? ln > rn : leftVal > rightVal;
					case '<=':
						return useNumeric ? ln <= rn : leftVal <= rightVal;
					case '>=':
						return useNumeric ? ln >= rn : leftVal >= rightVal;
					default:
						return false;
				}
			}
		}
	}

	// Resolve a value from a row, supporting dot notation for nested objects and JOIN prefixes
	private resolveValue(row: Row, path: string): any {
		if (path == null) return undefined;

		// Direct key match
		if (Object.prototype.hasOwnProperty.call(row, path)) {
			return row[path];
		}

		// Dot notation: try prefix matches then full traversal
		if (path.includes('.')) {
			const parts = path.split('.');

			// Try progressively longer prefixes as direct keys
			for (let i = 1; i < parts.length; i++) {
				const prefix = parts.slice(0, i).join('.');
				if (Object.prototype.hasOwnProperty.call(row, prefix)) {
					let current: any = row[prefix];
					for (let j = i; j < parts.length; j++) {
						if (current == null || typeof current !== 'object') return undefined;
						current = current[parts[j]];
					}
					if (current !== undefined) return current;
				}
			}

			// Full path traversal from root
			let current: any = row;
			for (const part of parts) {
				if (current == null || typeof current !== 'object') return undefined;
				current = current[part];
			}
			return current;
		}

		return undefined;
	}
}
