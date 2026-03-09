export function tokenize(query: string): string[] {
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

export function parseValueToken(token: string): any {
	if (token.toUpperCase() === 'NULL') return null;
	if (token.startsWith("'") && token.endsWith("'") && token.length >= 2) {
		return token.slice(1, -1);
	}
	const num = Number(token);
	if (!isNaN(num) && token !== '') return num;
	return token;
}
