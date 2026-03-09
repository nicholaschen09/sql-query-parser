import type { Condition, Row, SelectQuery } from '../types/types';
import { executeJoins } from './joins';
import { executeGroupBy, hasAggregates } from './grouping';
import { executeOrderBy } from './order-by';

interface ExecutionDeps {
	getTableData: (name: string) => Row[] | undefined;
	resolveValue: (row: Row, path: string) => any;
	evaluateCondition: (row: Row, condition: Condition) => boolean;
}

export function executeSelectQuery(query: SelectQuery, deps: ExecutionDeps): Row[] {
	// 1. Get base table data
	let results = deps.getTableData(query.table);
	if (!results) throw new Error(`Table '${query.table}' not found`);
	results = results.map((r) => ({ ...r }));

	// 2. Apply JOINs
	if (query.joins && query.joins.length > 0) {
		results = executeJoins(results, query.table, query.joins, {
			getTableData: deps.getTableData,
			evaluateCondition: deps.evaluateCondition
		});
	}

	// 3. Apply WHERE
	if (query.where) {
		results = results.filter((row) => deps.evaluateCondition(row, query.where!));
	}

	// 4. GROUP BY + aggregations
	const hasAgg = hasAggregates(query.columns);
	if (query.groupBy || hasAgg) {
		results = executeGroupBy(results, query.groupBy || [], query.columns, query.having, {
			resolveValue: deps.resolveValue,
			evaluateCondition: deps.evaluateCondition
		});
	} else {
		// 5. Column projection (no GROUP BY)
		if (query.columns.length > 0 && !(query.columns.length === 1 && query.columns[0] === '*')) {
			results = results.map((row) => {
				const newRow: Row = {};
				for (const col of query.columns) {
					if (typeof col === 'string') {
						const val = deps.resolveValue(row, col);
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
		results = executeOrderBy(results, query.orderBy, deps.resolveValue);
	}

	// 8. Apply LIMIT
	if (query.limit !== undefined) {
		results = results.slice(0, query.limit);
	}

	return results;
}
