import type { OrderByClause, Row } from '../types/types';

export function executeOrderBy(
	rows: Row[],
	orderBy: OrderByClause[],
	resolveValue: (row: Row, path: string) => any
): Row[] {
	return [...rows].sort((a, b) => {
		for (const ob of orderBy) {
			const av = resolveValue(a, ob.column);
			const bv = resolveValue(b, ob.column);
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
