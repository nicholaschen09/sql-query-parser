import type { Row } from '../types/types';

// Resolve a value from a row, supporting dot notation for nested objects and JOIN prefixes.
export function resolveValue(row: Row, path: string): any {
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
