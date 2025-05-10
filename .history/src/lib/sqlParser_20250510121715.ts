const { Row, SelectQuery, Condition, Operator } = require('./types');

class SQLParser {
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
            .split(/\s+/)
            .filter(token => token.length > 0);
    }

    private parseSelect(tokens: string[]): SelectQuery {
        if (tokens[0].toUpperCase() !== 'SELECT') {
            throw new Error('Query must start with SELECT');
        }

        let currentIndex = 1;
        const columns = this.parseColumns(tokens, currentIndex);
        currentIndex += columns.length + 1; // +1 for FROM

        if (tokens[currentIndex].toUpperCase() !== 'FROM') {
            throw new Error('Expected FROM clause');
        }
        currentIndex++;

        const table = tokens[currentIndex];
        currentIndex++;

        let where: Condition | undefined;
        let limit: number | undefined;

        if (currentIndex < tokens.length) {
            if (tokens[currentIndex].toUpperCase() === 'WHERE') {
                currentIndex++;
                where = this.parseCondition(tokens, currentIndex);
                currentIndex += this.getConditionLength(where);
            }

            if (currentIndex < tokens.length && tokens[currentIndex].toUpperCase() === 'LIMIT') {
                currentIndex++;
                limit = parseInt(tokens[currentIndex]);
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

    private parseColumns(tokens: string[], startIndex: number): string[] {
        const columns: string[] = [];
        let currentIndex = startIndex;

        while (currentIndex < tokens.length && tokens[currentIndex].toUpperCase() !== 'FROM') {
            if (tokens[currentIndex] === '*') {
                columns.push('*');
            } else {
                columns.push(tokens[currentIndex].replace(/,/g, ''));
            }
            currentIndex++;
        }

        return columns;
    }

    private parseCondition(tokens: string[], startIndex: number): Condition {
        let currentIndex = startIndex;
        const stack: (Condition | string)[] = [];

        while (currentIndex < tokens.length && tokens[currentIndex].toUpperCase() !== 'LIMIT') {
            const token = tokens[currentIndex];

            if (token === '(') {
                stack.push(token);
            } else if (token === ')') {
                const subCondition = this.evaluateStack(stack);
                stack.push(subCondition);
            } else if (['AND', 'OR'].includes(token.toUpperCase())) {
                stack.push(token.toUpperCase() as Operator);
            } else if (['=', '!=', '<', '>'].includes(token)) {
                const left = stack.pop();
                if (!left) throw new Error('Invalid condition');
                stack.push({
                    left,
                    operator: token as Operator,
                    right: this.parseValue(tokens[++currentIndex])
                });
            } else {
                stack.push(token);
            }

            currentIndex++;
        }

        return this.evaluateStack(stack);
    }

    private evaluateStack(stack: (Condition | string)[]): Condition {
        if (stack.length === 1) {
            const item = stack[0];
            if (typeof item === 'string') {
                throw new Error('Invalid condition');
            }
            return item;
        }

        const right = stack.pop();
        const operator = stack.pop() as Operator;
        const left = stack.pop();

        if (!left || !operator || !right) {
            throw new Error('Invalid condition');
        }

        return {
            left: typeof left === 'string' ? left : left,
            operator,
            right: typeof right === 'string' ? this.parseValue(right) : right
        };
    }

    private parseValue(value: string): string | number {
        if (value.startsWith("'") && value.endsWith("'")) {
            return value.slice(1, -1);
        }
        const num = Number(value);
        return isNaN(num) ? value : num;
    }

    private getConditionLength(condition: Condition): number {
        if (typeof condition.left === 'string') {
            return 3; // left operator right
        }
        return this.getConditionLength(condition.left) + 2 + this.getConditionLength(condition.right as Condition);
    }

    execute(query: SelectQuery): Row[] {
        let results = [...this.data];

        if (query.where) {
            results = results.filter(row => this.evaluateCondition(row, query.where!));
        }

        if (query.columns[0] !== '*') {
            results = results.map(row => {
                const newRow: Row = {};
                query.columns.forEach(col => {
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
        const left = typeof condition.left === 'string' ? row[condition.left] : this.evaluateCondition(row, condition.left);
        const right = typeof condition.right === 'object' ? this.evaluateCondition(row, condition.right) : condition.right;

        switch (condition.operator) {
            case '=':
                return left === right;
            case '!=':
                return left !== right;
            case '<':
                return typeof left === 'number' && typeof right === 'number' && left < right;
            case '>':
                return typeof left === 'number' && typeof right === 'number' && left > right;
            case 'AND':
                return Boolean(left) && Boolean(right);
            case 'OR':
                return Boolean(left) || Boolean(right);
            default:
                return false;
        }
    }
}

module.exports = { SQLParser }; 