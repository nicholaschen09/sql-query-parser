<script lang="ts">
	import { goto } from '$app/navigation';
</script>

<div class="page-wrapper">
	<main class="container">
		<button class="back-button" on:click={() => goto('/')}>← Back</button>

		<h1>How the SQL Query Parser Works</h1>

		<article class="blog-content">
			<p class="intro">
				This SQL query parser is implemented in <strong>four languages</strong> — TypeScript, Go, Rust, and
				Haskell — each using the same algorithm to transform SQL queries into executable operations on
				JSON data. The TypeScript version runs client-side in the browser, while Go, Rust, and Haskell
				each provide a server-side HTTP API. Here's how they all work under the hood, using the Go implementation
				as the reference.
			</p>
			<img
				src="/sql-string.png"
				alt="Example SQL query selecting data from a table with results shown"
				class="blog-image"
			/>

			<section>
				<h2>Architecture Overview</h2>
				<p>
					Every implementation follows the same three-stage pipeline: <strong>tokenization</strong>,
					<strong>parsing</strong>, and <strong>execution</strong>. A SQL string goes in, tokens
					come out of stage one, an abstract syntax tree (AST) comes out of stage two, and query
					results come out of stage three.
				</p>
				<img
					src="/parser.png"
					alt="Parser architecture: grammar flows into parser generator, which produces a parser that takes tokens and outputs an intermediate representation"
					class="blog-image"
				/>
				<p>
					Each language defines the same core structure — an <code>SQLParser</code> that holds data
					and exposes
					<code>parse</code> and <code>execute</code> methods:
				</p>
				<div class="code-label">Go</div>
				<pre class="code-block">type SQLParser struct {'{'}
    data []Row
{'}'}

func (p *SQLParser) Parse(query string) (*SelectQuery, error) {'{'}...{'}'}
func (p *SQLParser) Execute(query *SelectQuery) ([]Row, error) {'{'}...{'}'}</pre>
			</section>

			<section>
				<h2>1. Tokenization</h2>
				<p>
					The first thing the parser does is break the raw SQL string into tokens. The tokenizer:
				</p>
				<ul>
					<li>Adds spaces around parentheses and commas for easier splitting</li>
					<li>Splits the query by whitespace</li>
					<li>Removes trailing semicolons</li>
					<li>Filters out empty tokens</li>
				</ul>
				<pre class="code-block">SELECT state, pop FROM table WHERE pop > 5000
// Becomes: ["SELECT", "state", ",", "pop", "FROM", "table", "WHERE", "pop", ">", "5000"]</pre>
				<img
					src="/tokenization.png"
					alt="Tokenization: sentences are broken down into individual word tokens"
					class="blog-image"
				/>
				<div class="code-label">Go</div>
				<pre class="code-block">func (p *SQLParser) tokenize(query string) []string {'{'}
    query = strings.ReplaceAll(query, "(", " ( ")
    query = strings.ReplaceAll(query, ")", " ) ")
    query = strings.ReplaceAll(query, ",", " , ")
    query = strings.TrimSuffix(query, ";")
    return strings.Fields(query)
{'}'}</pre>
			</section>

			<section>
				<h2>2. Parsing the SELECT Statement</h2>
				<p>
					Next, the parser walks through the token array and identifies the key components of the
					query:
				</p>
				<ul>
					<li>
						<strong>SELECT</strong> — Must be the first token, otherwise the parser throws an error
					</li>
					<li>
						<strong>Columns</strong> — Everything between SELECT and FROM (supports <code>*</code> or
						a comma-separated list)
					</li>
					<li>
						<strong>FROM</strong> — Required keyword that separates columns from the table name
					</li>
					<li><strong>Table name</strong> — The token immediately after FROM</li>
					<li><strong>WHERE</strong> — Optional condition clause, parsed next if present</li>
					<li><strong>LIMIT</strong> — Optional row limit, parsed last if present</li>
				</ul>
				<div class="code-label">Go</div>
				<pre
					class="code-block">func (p *SQLParser) parseSelect(tokens []string) (*SelectQuery, error) {'{'}
    if strings.ToUpper(tokens[0]) != "SELECT" {'{'}
        return nil, fmt.Errorf("query must start with SELECT")
    {'}'}

    // Find FROM index
    fromIndex := -1
    for i, token := range tokens {'{'}
        if strings.ToUpper(token) == "FROM" {'{'}
            fromIndex = i
            break
        {'}'}
    {'}'}
    if fromIndex == -1 {'{'}
        return nil, fmt.Errorf("expected FROM clause")
    {'}'}

    // Parse columns between SELECT and FROM
    columns := p.parseColumns(tokens, 1, fromIndex)
    table := tokens[fromIndex + 1]

    // ... parse optional WHERE and LIMIT clauses
{'}'}</pre>
			</section>

			<section>
				<h2>3. Parsing WHERE Conditions</h2>
				<p>
					If a WHERE keyword is found, the parser uses <strong>recursive descent parsing</strong> to build
					a tree of conditions. Three functions call each other to handle operator precedence:
				</p>
				<ul>
					<li>
						<strong>parseConditionRecursive</strong> — handles <code>OR</code> (lowest precedence, evaluated
						last)
					</li>
					<li><strong>parseAnd</strong> — handles <code>AND</code> (higher precedence)</li>
					<li>
						<strong>parseOperand</strong> — handles comparisons (<code>=</code>, <code>!=</code>,
						<code>&lt;</code>, <code>&gt;</code>) and parenthesized groups (highest precedence)
					</li>
				</ul>
				<pre class="code-block">pop > 3000 OR (region = 'West' AND pop > 500)
// Parsed as: OR(
//   pop > 3000,
//   AND(region = 'West', pop > 500)
// )</pre>
				<div class="code-label">Go</div>
				<pre class="code-block">// Parse OR (lowest precedence)
func (p *SQLParser) parseConditionRecursive(tokens []string, idx int) (*Condition, int, error) {'{'}
    left, nextIdx, err := p.parseAnd(tokens, idx)
    if err != nil {'{'}
        return nil, 0, err
    {'}'}
    for nextIdx &lt; len(tokens) &amp;&amp; strings.ToUpper(tokens[nextIdx]) == "OR" {'{'}
        right, afterRight, err := p.parseAnd(tokens, nextIdx+1)
        if err != nil {'{'}
            return nil, 0, err
        {'}'}
        left = &amp;Condition{'{'}Left: left, Operator: "OR", Right: right{'}'}
        nextIdx = afterRight
    {'}'}
    return left, nextIdx, nil
{'}'}

// Parse AND (higher precedence)
func (p *SQLParser) parseAnd(tokens []string, idx int) (*Condition, int, error) {'{'}
    left, nextIdx, err := p.parseOperand(tokens, idx)
    if err != nil {'{'}
        return nil, 0, err
    {'}'}
    for nextIdx &lt; len(tokens) &amp;&amp; strings.ToUpper(tokens[nextIdx]) == "AND" {'{'}
        right, afterRight, err := p.parseOperand(tokens, nextIdx+1)
        if err != nil {'{'}
            return nil, 0, err
        {'}'}
        left = &amp;Condition{'{'}Left: left, Operator: "AND", Right: right{'}'}
        nextIdx = afterRight
    {'}'}
    return left, nextIdx, nil
{'}'}</pre>
			</section>

			<section>
				<h2>4. The AST — What Parsing Produces</h2>
				<p>
					The output of steps 2 and 3 is an <strong>Abstract Syntax Tree</strong> (AST) — a tree-shaped
					data structure that represents the query's structure. Instead of a flat string, the parser has
					now produced a tree of nested objects that the executor can walk through.
				</p>
				<p>A flat SQL string like this:</p>
				<pre
					class="code-block">SELECT state, pop FROM table WHERE pop > 5000 AND region = 'West'</pre>
				<p>Has been transformed into this structured tree:</p>
				<pre class="code-block">SelectQuery
├── type: "SELECT"
├── columns: ["state", "pop"]
├── table: "table"
└── where:
          AND
         /   \
        /     \
    pop>5000   region='West'</pre>
				<p>
					<strong>Why a tree?</strong> Because it captures nesting and precedence. Consider the
					expression
					<code>A OR B AND C</code>. This means <code>A OR (B AND C)</code>, not
					<code>(A OR B) AND C</code>. The tree naturally encodes this:
				</p>
				<pre class="code-block">      OR
     /  \
    A   AND
       /   \
      B     C</pre>
				<p>
					<code>AND</code> is deeper in the tree, so it gets evaluated first. The executor walks the tree
					bottom-up — it evaluates the leaf nodes, then combines results as it moves back up to the root.
				</p>
				<p>
					In Go, each node is a <code>Condition</code> struct where <code>Left</code> and
					<code>Right</code>
					can themselves be <code>*Condition</code> pointers. That recursion is what makes it a tree:
				</p>
				<div class="code-label">Go</div>
				<pre class="code-block">type Condition struct {'{'}
    Left     interface{'{'}{'}'} // string or *Condition
    Operator Operator
    Right    interface{'{'}{'}'} // string, float64, or *Condition
{'}'}</pre>
				<img
					src="/ast.png"
					alt="Abstract syntax tree: expressions are parsed into a tree structure with operators and operands"
					class="blog-image"
				/>
			</section>

			<section>
				<h2>5. Execution</h2>
				<p>
					Now the executor takes the AST and runs it against the JSON data. It performs three
					operations in order:
				</p>
				<ol>
					<li>
						<strong>Filter</strong> — Apply WHERE conditions row by row, keeping only matching rows
					</li>
					<li>
						<strong>Project</strong> — Select only the requested columns (or keep all columns with
						<code>*</code>)
					</li>
					<li><strong>Limit</strong> — Truncate results to the specified number of rows</li>
				</ol>
				<div class="code-label">Go</div>
				<pre class="code-block">func (p *SQLParser) Execute(query *SelectQuery) ([]Row, error) {'{'}
    results := make([]Row, len(p.data))
    copy(results, p.data)

    // 1. Filter — apply WHERE conditions
    if query.Where != nil {'{'}
        filtered := []Row{'{'}{'}'}
        for _, row := range results {'{'}
            if p.evaluateCondition(row, query.Where) {'{'}
                filtered = append(filtered, row)
            {'}'}
        {'}'}
        results = filtered
    {'}'}

    // 2. Project — select only requested columns
    if len(query.Columns) > 0 &amp;&amp; query.Columns[0] != "*" {'{'}
        for i, row := range results {'{'}
            newRow := make(Row)
            for _, col := range query.Columns {'{'}
                if val, ok := row[col]; ok {'{'}
                    newRow[col] = val
                {'}'}
            {'}'}
            results[i] = newRow
        {'}'}
    {'}'}

    // 3. Limit — truncate results
    if query.Limit != nil {'{'}
        limit := *query.Limit
        if limit &lt; len(results) {'{'}
            results = results[:limit]
        {'}'}
    {'}'}

    return results, nil
{'}'}</pre>
			</section>

			<section>
				<h2>6. Condition Evaluation</h2>
				<p>
					During the filter step, each row is tested against the WHERE condition tree. The executor
					walks the AST recursively:
				</p>
				<ul>
					<li>
						If the node is <code>AND</code> or <code>OR</code> — evaluate both child nodes, combine
						with <code>&amp;&amp;</code> or <code>||</code>
					</li>
					<li>
						If the node is a comparison (<code>=</code>, <code>!=</code>, <code>&lt;</code>,
						<code>&gt;</code>) — look up the column value from the row and compare
					</li>
					<li>String literals (in single quotes) are extracted as strings</li>
					<li>Numeric values are parsed automatically</li>
					<li>Column-to-column comparisons are also supported</li>
				</ul>
				<div class="code-label">Go</div>
				<pre
					class="code-block">func (p *SQLParser) evaluateCondition(row Row, cond *Condition) bool {'{'}
    // Handle logical operators (AND, OR)
    if cond.Operator == "AND" || cond.Operator == "OR" {'{'}
        left := p.evaluateCondition(row, cond.Left.(*Condition))
        right := p.evaluateCondition(row, cond.Right.(*Condition))
        if cond.Operator == "AND" {'{'}
            return left &amp;&amp; right
        {'}'}
        return left || right
    {'}'}

    // Handle comparison operators (=, !=, &lt;, >)
    leftVal := p.resolveValue(row, cond.Left)
    rightVal := p.resolveValue(row, cond.Right)

    switch cond.Operator {'{'}
    case "=":
        return leftVal == rightVal
    case "!=":
        return leftVal != rightVal
    case "&lt;":
        return leftVal &lt; rightVal
    case ">":
        return leftVal > rightVal
    {'}'}
    return false
{'}'}</pre>
				<p>
					The result bubbles back up the tree: each leaf returns <code>true</code> or
					<code>false</code>, and the logical operators combine them until the root node produces a
					final boolean for that row.
				</p>
			</section>

			<section>
				<h2>Example Walkthrough</h2>
				<p>Let's trace through a complete query to see all six steps in action:</p>
				<pre class="code-block">SELECT state, pop FROM table WHERE pop > 5000</pre>
				<p><strong>Step 1 — Tokenize:</strong></p>
				<pre
					class="code-block">["SELECT", "state", ",", "pop", "FROM", "table", "WHERE", "pop", ">", "5000"]</pre>
				<p><strong>Step 2 — Parse SELECT:</strong></p>
				<pre class="code-block">columns: ["state", "pop"]
table: "table"</pre>
				<p><strong>Step 3 — Parse WHERE:</strong></p>
				<pre class="code-block">{'{'}
  left: "pop",
  operator: ">",
  right: 5000
{'}'}</pre>
				<p><strong>Step 4 — AST complete:</strong></p>
				<pre class="code-block">{'{'}
  type: "SELECT",
  columns: ["state", "pop"],
  table: "table",
  where: {'{'} left: "pop", operator: ">", right: 5000 {'}'}
{'}'}</pre>
				<p><strong>Step 5 — Execute:</strong></p>
				<p>
					For each row, check if <code>pop > 5000</code>. Keep matching rows, then select only the
					<code>state</code>
					and <code>pop</code> columns.
				</p>
			</section>

			<section>
				<h2>Limitations</h2>
				<p>This parser is intentionally minimal and supports:</p>
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
		</article>
	</main>
	<footer>
		Made by <a
			href="https://x.com/nicholaschen__"
			target="_blank"
			style="color:black;text-decoration:underline;">Nicholas Chen</a
		>
		&middot;
		<a
			href="https://github.com/nicholaschen09"
			target="_blank"
			style="color:black;text-decoration:underline;">GitHub</a
		>
	</footer>
</div>

<style>
	.back-button {
		padding: 0.5rem 1rem;
		background: #fafafa;
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
		background: #f0f0f0;
	}

	.blog-content {
		width: 100%;
	}

	.blog-content section {
		margin-bottom: 2.5rem;
	}

	.blog-content h2 {
		font-size: 20px;
		font-weight: 500;
		margin-bottom: 0.75rem;
		margin-top: 2rem;
	}

	.blog-content h3 {
		font-size: 16px;
		font-weight: 500;
		margin-top: 1.25rem;
		margin-bottom: 0.5rem;
	}

	.blog-content p {
		font-size: 15px;
		margin-bottom: 1rem;
		line-height: 1.7;
	}

	.blog-content .intro {
		font-size: 16px;
		margin-bottom: 2rem;
	}

	.blog-content ul,
	.blog-content ol {
		margin-left: 1.5rem;
		margin-bottom: 1rem;
	}

	.blog-content li {
		font-size: 15px;
		margin-bottom: 0.5rem;
		line-height: 1.7;
	}

	.blog-content li code,
	.blog-content p code {
		font-family: 'JetBrains Mono', monospace;
		font-size: 14px;
		background: #f0f0f0;
		padding: 0.15rem 0.4rem;
		border: 1px solid #ddd;
	}

	.code-block {
		background: #fafafa;
		border: 1px solid black;
		padding: 1.25rem 1.5rem;
		font-family: 'JetBrains Mono', monospace;
		font-size: 13px;
		line-height: 1.7;
		overflow-x: auto;
		margin: 0 0 1.25rem 0;
		white-space: pre;
		letter-spacing: 0.3px;
	}

	.code-label {
		font-family: 'JetBrains Mono', monospace;
		font-size: 11px;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 1px;
		color: #555;
		margin-top: 1rem;
		margin-bottom: 0;
		padding: 0.35rem 0.75rem;
		background: #f0f0f0;
		border: 1px solid black;
		border-bottom: none;
		display: inline-block;
	}

	.code-label + .code-block {
		margin-top: 0;
	}

	.blog-image {
		display: block;
		width: 100%;
		margin: 1.5rem 0;
		border: 1px solid black;
		padding: 1rem;
		background: #fafafa;
		box-sizing: border-box;
	}

	.blog-content strong {
		font-weight: 600;
	}
</style>
