<script lang="ts">
    import { goto } from '$app/navigation';
</script>

<div class="page-wrapper">
<main class="container">
    <button class="back-button" on:click={() => goto('/')}>← Back</button>
    
    <h1>How the SQL Parser Works</h1>
    
    <article class="blog-content">
        <p class="intro">
            This SQL parser is a minimal implementation that transforms SQL queries into executable operations on JSON data. 
            Here's how it works under the hood.
        </p>

        <section>
            <h2>Architecture Overview</h2>
            <p>
                The parser consists of two main phases: <strong>parsing</strong> and <strong>execution</strong>. 
                The parsing phase converts a SQL string into an abstract syntax tree (AST), and the execution phase 
                runs that AST against the provided JSON data.
            </p>
        </section>

        <section>
            <h2>1. Tokenization</h2>
            <p>
                The first step is breaking down the SQL query into tokens. The parser:
            </p>
            <ul>
                <li>Adds spaces around parentheses and commas for easier splitting</li>
                <li>Splits the query by whitespace</li>
                <li>Removes trailing semicolons</li>
                <li>Filters out empty tokens</li>
            </ul>
            <pre class="code-block">SELECT state, pop FROM table WHERE pop > 5000
// Becomes: ["SELECT", "state", ",", "pop", "FROM", "table", "WHERE", "pop", ">", "5000"]</pre>
        </section>

        <section>
            <h2>2. Parsing the SELECT Statement</h2>
            <p>
                The parser identifies key components:
            </p>
            <ul>
                <li><strong>SELECT</strong> - Must be the first token</li>
                <li><strong>Columns</strong> - Everything between SELECT and FROM (supports * or comma-separated list)</li>
                <li><strong>FROM</strong> - Required keyword</li>
                <li><strong>Table name</strong> - The token immediately after FROM</li>
                <li><strong>WHERE</strong> - Optional condition clause</li>
                <li><strong>LIMIT</strong> - Optional row limit</li>
            </ul>
        </section>

        <section>
            <h2>3. Parsing WHERE Conditions</h2>
            <p>
                The WHERE clause uses recursive descent parsing to handle operator precedence:
            </p>
            <ul>
                <li><strong>OR</strong> has the lowest precedence (evaluated last)</li>
                <li><strong>AND</strong> has higher precedence</li>
                <li><strong>Parentheses</strong> override precedence</li>
                <li><strong>Comparison operators</strong> (=, !=, &lt;, &gt;) have the highest precedence</li>
            </ul>
            <pre class="code-block">pop > 3000 OR (region = 'West' AND pop > 500)
// Parsed as: OR(
//   pop > 3000,
//   AND(region = 'West', pop > 500)
// )</pre>
        </section>

        <section>
            <h2>4. Execution</h2>
            <p>
                Once parsed, the query is executed against the data:
            </p>
            <ol>
                <li><strong>Filter</strong> - Apply WHERE conditions row by row</li>
                <li><strong>Project</strong> - Select only requested columns (or all with *)</li>
                <li><strong>Limit</strong> - Truncate results to the specified number</li>
            </ol>
        </section>

        <section>
            <h2>5. Condition Evaluation</h2>
            <p>
                Conditions are evaluated recursively:
            </p>
            <ul>
                <li>Logical operators (AND/OR) evaluate left and right sides</li>
                <li>Comparison operators compare values (supports column-to-column comparisons)</li>
                <li>String literals are extracted from single quotes</li>
                <li>Numbers are parsed automatically</li>
            </ul>
        </section>

        <section>
            <h2>Implementation Details</h2>
            <h3>TypeScript Parser</h3>
            <p>
                The TypeScript implementation runs entirely client-side in the browser. It uses:
            </p>
            <ul>
                <li>Recursive descent parsing for WHERE clauses</li>
                <li>Array methods (filter, map, slice) for data manipulation</li>
                <li>No external dependencies - pure JavaScript</li>
            </ul>

            <h3>Go Parser</h3>
            <p>
                The Go implementation provides a REST API endpoint. It:
            </p>
            <ul>
                <li>Uses the same parsing algorithm for consistency</li>
                <li>Runs server-side for better performance on large datasets</li>
                <li>Returns JSON responses to the frontend</li>
            </ul>
        </section>

        <section>
            <h2>Limitations</h2>
            <p>
                This parser is intentionally minimal and supports:
            </p>
            <ul>
                <li>✓ SELECT queries only</li>
                <li>✓ WHERE with AND, OR, parentheses</li>
                <li>✓ Comparison operators: =, !=, &lt;, &gt;</li>
                <li>✓ LIMIT clause</li>
                <li>✓ Column selection (* or specific columns)</li>
            </ul>
            <p>
                It does <strong>not</strong> support:
            </p>
            <ul>
                <li>✗ JOINs</li>
                <li>✗ GROUP BY</li>
                <li>✗ Subqueries</li>
                <li>✗ Aggregations (SUM, COUNT, etc.)</li>
                <li>✗ NULL values</li>
                <li>✗ Nested objects or arrays</li>
            </ul>
        </section>

        <section>
            <h2>Example Walkthrough</h2>
            <p>
                Let's trace through a simple query:
            </p>
            <pre class="code-block">SELECT state, pop FROM table WHERE pop > 5000</pre>
            <ol>
                <li><strong>Tokenize:</strong> ["SELECT", "state", ",", "pop", "FROM", "table", "WHERE", "pop", ">", "5000"]</li>
                <li><strong>Parse columns:</strong> ["state", "pop"]</li>
                <li><strong>Parse table:</strong> "table"</li>
                <li><strong>Parse WHERE:</strong> {left: "pop", operator: ">", right: 5000}</li>
                <li><strong>Execute:</strong> Filter rows where pop > 5000, then select only state and pop columns</li>
            </ol>
        </section>
    </article>
</main>
</div>

<style>
    .back-button {
        padding: 0.5rem 1rem;
        background: white;
        color: black;
        border: 1px solid black;
        border-radius: 0;
        font-size: 12px;
        font-weight: 400;
        cursor: pointer;
        margin-bottom: 2rem;
        font-family: inherit;
    }

    .back-button:hover {
        background: #f5f5f5;
    }

    .blog-content {
        max-width: 700px;
        margin: 0 auto;
    }

    .blog-content section {
        margin-bottom: 2rem;
    }

    .blog-content h2 {
        font-size: 18px;
        font-weight: 400;
        margin-bottom: 0.5rem;
        margin-top: 1.5rem;
    }

    .blog-content h3 {
        font-size: 14px;
        font-weight: 400;
        margin-top: 1rem;
        margin-bottom: 0.5rem;
    }

    .blog-content p {
        margin-bottom: 1rem;
        line-height: 1.6;
    }

    .blog-content .intro {
        font-size: 14px;
        margin-bottom: 2rem;
    }

    .blog-content ul,
    .blog-content ol {
        margin-left: 1.5rem;
        margin-bottom: 1rem;
    }

    .blog-content li {
        margin-bottom: 0.5rem;
        line-height: 1.6;
    }

    .code-block {
        background: white;
        border: 1px solid black;
        padding: 1rem;
        font-family: 'Courier New', monospace;
        font-size: 11px;
        overflow-x: auto;
        margin: 1rem 0;
        white-space: pre;
    }

    .blog-content strong {
        font-weight: 400;
    }
</style>
