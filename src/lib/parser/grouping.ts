import type { AggregateColumn, ColumnExpr, Condition, Row } from '../types/types';

interface GroupingDeps {
	resolveValue: (row: Row, path: string) => any;
	evaluateCondition: (row: Row, condition: Condition) => boolean;
}

export function hasAggregates(columns: ColumnExpr[]): boolean {
	return columns.some((c) => typeof c === 'object' && 'func' in c);
}

export function executeGroupBy(
	rows: Row[],
	groupBy: string[],
	columns: ColumnExpr[],
	having: Condition | undefined,
	deps: GroupingDeps
): Row[] {
	const groups = new Map<string, Row[]>();

	if (groupBy.length === 0) {
		groups.set('__all__', rows);
	} else {
		for (const row of rows) {
			const key = groupBy
				.map((col) => JSON.stringify(deps.resolveValue(row, col) ?? null))
				.join('|');
			if (!groups.has(key)) groups.set(key, []);
			groups.get(key)!.push(row);
		}
	}

	let results: Row[] = [];
	for (const [, groupRows] of groups) {
		const newRow: Row = {};

		for (const col of groupBy) {
			newRow[col] = deps.resolveValue(groupRows[0], col);
		}

		for (const col of columns) {
			if (typeof col === 'object' && 'func' in col) {
				const agg = col as AggregateColumn;
				const alias = agg.alias || `${agg.func}(${agg.column})`;
				newRow[alias] = computeAggregate(agg.func, agg.column, groupRows, deps.resolveValue);
			} else if (typeof col === 'string' && col !== '*' && !groupBy.includes(col)) {
				newRow[col] = deps.resolveValue(groupRows[0], col);
			}
		}

		results.push(newRow);
	}

	if (having) {
		results = results.filter((row) => deps.evaluateCondition(row, having));
	}

	return results;
}

function computeAggregate(
	func: string,
	column: string,
	rows: Row[],
	resolveValue: (row: Row, path: string) => any
): any {
	switch (func) {
		case 'COUNT':
			if (column === '*') return rows.length;
			return rows.filter((r) => resolveValue(r, column) != null).length;
		case 'SUM': {
			let sum = 0;
			for (const r of rows) {
				const v = resolveValue(r, column);
				if (typeof v === 'number') sum += v;
			}
			return sum;
		}
		case 'AVG': {
			let sum = 0,
				count = 0;
			for (const r of rows) {
				const v = resolveValue(r, column);
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
				const v = resolveValue(r, column);
				if (v != null && (min === null || v < min)) min = v;
			}
			return min;
		}
		case 'MAX': {
			let max: any = null;
			for (const r of rows) {
				const v = resolveValue(r, column);
				if (v != null && (max === null || v > max)) max = v;
			}
			return max;
		}
		default:
			return null;
	}
}
