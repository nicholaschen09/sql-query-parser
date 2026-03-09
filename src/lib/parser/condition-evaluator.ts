import type { Condition, Row, SelectQuery } from '../types/types';

interface ConditionEvaluatorDeps {
	resolveValue: (row: Row, path: string) => any;
	executeSubquery: (query: SelectQuery) => Row[];
}

export function evaluateCondition(
	row: Row,
	condition: Condition,
	deps: ConditionEvaluatorDeps
): boolean {
	// Logical operators
	if (condition.operator === 'AND' || condition.operator === 'OR') {
		const left = evaluateCondition(row, condition.left as Condition, deps);
		const right = evaluateCondition(row, condition.right as Condition, deps);
		return condition.operator === 'AND' ? left && right : left || right;
	}

	// Resolve left value
	let leftVal: any;
	if (typeof condition.left === 'string') {
		const resolved = deps.resolveValue(row, condition.left);
		leftVal = resolved !== undefined ? resolved : condition.left;
	} else {
		leftVal = evaluateCondition(row, condition.left, deps);
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
				const subResults = deps.executeSubquery(condition.right as SelectQuery);
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
				const subResults = deps.executeSubquery(condition.right as SelectQuery);
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
				const resolved = deps.resolveValue(row, condition.right);
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
			const useNumeric = !isNaN(ln) && !isNaN(rn) && leftVal !== '' && rightVal !== '';

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
