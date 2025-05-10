import type { Row, SelectQuery, Condition, Operator } from '../types/types';

export class SQLParser {
    private data: Row[];

    constructor(data: Row[]) {
        this.data = data;
    }

    parse(query: string): SelectQuery {
        const tokens = this.tokenize(query);
        return this.parseSelect(tokens);
    }

    private tokenize(query: string): string[] {
        return query
            .replace(/\(/g, ' ( ')
            .replace(/\)/g, ' ) ')
            .replace(/,/g, ' , ')
            .split(/\s+/)
            .map(token => token.replace(/;$/, ''))
            .filter(token => token.length > 0);
    }

    private parseSelect(tokens: string[]): SelectQuery {
        if (tokens[0].toUpperCase() !== 'SELECT') {
            throw new Error('Query must start with SELECT');
        }

        // Find the index of FROM
        const fromIndex = tokens.findIndex(token => token.toUpperCase() === 'FROM');
        if (fromIndex === -1) {
            throw new Error('Expected FROM clause');
        }

        // Columns are everything between SELECT and FROM
        const columns = this.parseColumns(tokens, 1, fromIndex);
        if (columns.length === 0 || (columns.length === 1 && columns[0] === '')) {
            throw new Error('No columns specified in SELECT');
        }

        // Table name is the token after FROM
        if (fromIndex + 1 >= tokens.length) {
            throw new Error('Expected table name after FROM');
        }
        const table = tokens[fromIndex + 1];

        let currentIndex = fromIndex + 2;
        let where: Condition | undefined;
        let limit: number | undefined;

        if (currentIndex < tokens.length && tokens[currentIndex].toUpperCase() === 'WHERE') {
            currentIndex++;
            const [cond, nextIdx] = this.parseConditionRecursive(tokens, currentIndex);
            where = cond;
            currentIndex = nextIdx;
        }

        if (currentIndex < tokens.length && tokens[currentIndex].toUpperCase() === 'LIMIT') {
            currentIndex++;
            limit = parseInt(tokens[currentIndex]);
            if (isNaN(limit) || limit < 0) {
                throw new Error('LIMIT must be a non-negative integer');
            }
        }

        return {
            type: 'SELECT',
            columns,
            table,
            where,
            limit
        };
    }

    private parseColumns(tokens: string[], startIndex: number, endIndex: number): string[] {
        const columns: string[] = [];
        for (let i = startIndex; i < endIndex; i++) {
            if (tokens[i] === ',' || tokens[i] === '') continue;
            columns.push(tokens[i].replace(/,/g, ''));
        }
        return columns;
    }

    // Parse OR (lowest precedence)
    private parseConditionRecursive(tokens: string[], idx: number): [Condition, number] {
        let [left, nextIdx] = this.parseAnd(tokens, idx);
        while (nextIdx < tokens.length && tokens[nextIdx]?.toUpperCase() === 'OR') {
            const operator = tokens[nextIdx].toUpperCase() as Operator;
            const [right, afterRight] = this.parseAnd(tokens, nextIdx + 1);
            left = { left, operator, right };
            nextIdx = afterRight;
        }
        return [left, nextIdx];
    }

    // Parse AND (higher precedence)
    private parseAnd(tokens: string[], idx: number): [Condition, number] {
        let [left, nextIdx] = this.parseOperand(tokens, idx);
        while (nextIdx < tokens.length && tokens[nextIdx]?.toUpperCase() === 'AND') {
            const operator = tokens[nextIdx].toUpperCase() as Operator;
            const [right, afterRight] = this.parseOperand(tokens, nextIdx + 1);
            left = { left, operator, right };
            nextIdx = afterRight;
        }
        return [left, nextIdx];
    }

    // Parse a single operand or parenthesized expression
    private parseOperand(tokens: string[], idx: number): [Condition, number] {
        if (tokens[idx] === '(') {
            const [cond, nextIdx] = this.parseConditionRecursive(tokens, idx + 1);
            if (tokens[nextIdx] !== ')') {
                throw new Error('Expected closing parenthesis');
            }
            return [cond, nextIdx + 1];
        }
        // Parse left operand
        const left = tokens[idx];
        const operator = tokens[idx + 1] as Operator;
        const rightToken = tokens[idx + 2];
        let right: string | number;
        if (rightToken?.startsWith("'")) {
            right = rightToken.slice(1, -1);
        } else if (!isNaN(Number(rightToken))) {
            right = Number(rightToken);
        } else {
            right = rightToken;
        }
        return [{ left, operator, right }, idx + 3];
    }

    execute(query: SelectQuery): Row[] {
        let results = [...this.data];

        if (query.where) {
            results = results.filter(row => this.evaluateCondition(row, query.where!));
        }

        if (query.columns[0] !== '*') {
            results = results.map(row => {
                const newRow: Row = {};
                query.columns.forEach((col: string) => {
                    if (col in row) {
                        newRow[col] = row[col];
                    }
                });
                return newRow;
            });
        }

        if (query.limit) {
            results = results.slice(0, query.limit);
        }

        return results;
    }

    private evaluateCondition(row: Row, condition: Condition): boolean {
        if ('operator' in condition && (condition.operator === 'AND' || condition.operator === 'OR')) {
            const left = this.evaluateCondition(row, condition.left as Condition);
            const right = this.evaluateCondition(row, condition.right as Condition);
            if (condition.operator === 'AND') return left && right;
            if (condition.operator === 'OR') return left || right;
        } else {
            // Support column-to-column and column-to-value
            const leftVal = typeof condition.left === 'string' ? (row[condition.left] ?? condition.left) : this.evaluateCondition(row, condition.left);
            let rightVal: any;
            if (
                typeof condition.right === 'string' &&
                Object.prototype.hasOwnProperty.call(row, condition.right)
            ) {
                rightVal = row[condition.right];
            } else {
                rightVal = condition.right;
            }
            switch (condition.operator) {
                case '=':
                    return leftVal === rightVal;
                case '!=':
                    return leftVal !== rightVal;
                case '<':
                    return leftVal < rightVal;
                case '>':
                    return leftVal > rightVal;
                default:
                    return false;
            }
        }
        return false;
    }
} 