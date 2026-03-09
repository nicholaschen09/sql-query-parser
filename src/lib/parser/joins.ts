import type { Condition, JoinClause, Row } from '../types/types';

interface JoinExecutionDeps {
	getTableData: (name: string) => Row[] | undefined;
	evaluateCondition: (row: Row, condition: Condition) => boolean;
}

export function executeJoins(
	leftRows: Row[],
	leftTable: string,
	joins: JoinClause[],
	deps: JoinExecutionDeps
): Row[] {
	let results = leftRows;

	for (const join of joins) {
		const rightData = deps.getTableData(join.table);
		if (!rightData) throw new Error(`Table '${join.table}' not found for JOIN`);

		const joined: Row[] = [];

		if (join.joinType === 'RIGHT') {
			for (const rightRow of rightData) {
				let matched = false;
				for (const leftRow of results) {
					const merged = mergeRows(leftRow, rightRow, leftTable, join.table);
					if (deps.evaluateCondition(merged, join.on)) {
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
					const merged = mergeRows(leftRow, rightRow, leftTable, join.table);
					if (deps.evaluateCondition(merged, join.on)) {
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

function mergeRows(leftRow: Row, rightRow: Row, leftTable: string, rightTable: string): Row {
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
