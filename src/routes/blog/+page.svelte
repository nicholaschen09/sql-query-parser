<div class="page-wrapper">
	<main class="container">
		<a class="back-button" href="/">← Back</a>

		<h1>How the SQL Query Parser Works</h1>

		<article class="blog-content">
			<p class="intro">
				This app uses a <strong>TypeScript-only</strong> SQL parser. Queries are parsed and executed in the
				browser against JSON tables, so the whole flow is deterministic, inspectable, and fast for local
				datasets.
			</p>
			<img
				src="/sql-string.png"
				alt="Example SQL query selecting data from a table with results shown"
				class="blog-image"
			/>

			<section>
				<h2>Architecture Overview</h2>
				<p>
					The parser is split into three stages: tokenization, parsing, and execution. The parser converts a
					SQL string into an AST (<code>SelectQuery</code>), then the executor applies clauses in SQL order.
				</p>
				<img
					src="/parser.png"
					alt="Parser architecture: SQL input flows through tokenization, parsing, and execution"
					class="blog-image"
				/>
				<h3>End-to-End Flow</h3>
				<ol>
					<li>Read query string and convert it to token list.</li>
					<li>Parse tokens into a typed query object.</li>
					<li>Load base table rows.</li>
					<li>Apply JOINs, WHERE, GROUP BY/HAVING, projection, ORDER BY, and LIMIT.</li>
					<li>Return final rows for rendering/export.</li>
				</ol>
				<div class="code-label">TypeScript API</div>
				<pre class="code-block">const parser = new SQLParser(tables);
const ast = parser.parse(sql);
const rows = parser.execute(ast);</pre>
			</section>

			<section>
				<h2>1. Tokenization</h2>
				<p>
					Tokenization scans character-by-character. It preserves quoted string literals, recognizes two-char
					operators (<code>&gt;=</code>, <code>&lt;=</code>, <code>!=</code>), and emits punctuation tokens
					(<code>(</code>, <code>)</code>, <code>,</code>, <code>=</code>, <code>&lt;</code>,
					<code>&gt;</code>, <code>*</code>).
				</p>
				<h3>Important Behavior</h3>
				<ul>
					<li>Single quotes are treated as string delimiters and may contain spaces.</li>
					<li>Trailing semicolon is ignored.</li>
					<li>Dotted identifiers like <code>users.id</code> are kept as one token.</li>
					<li>Whitespace is fully normalized during scanning.</li>
				</ul>
				<div class="code-label">Tokenizer Example</div>
				<pre class="code-block">SELECT users.name, SUM(orders.amount) AS total
FROM users
LEFT JOIN orders ON users.id = orders.user_id
WHERE users.name LIKE 'A%'
GROUP BY users.name
HAVING total &gt;= 100
ORDER BY total DESC
LIMIT 5;</pre>
				<p>Becomes a token stream consumed by the parser in a single left-to-right pass.</p>
				<img
					src="/tokenization.png"
					alt="Tokenization: query text broken into SQL tokens"
					class="blog-image"
				/>
			</section>

			<section>
				<h2>2. Parsing Into an AST</h2>
				<p>
					The parser validates that queries start with <code>SELECT</code>, finds the top-level
					<code>FROM</code> boundary (while skipping nested parentheses), and then parses optional clauses in
					order:
				</p>
				<ol>
					<li><code>JOIN</code> blocks (<code>INNER</code>, <code>LEFT</code>, <code>RIGHT</code>)</li>
					<li><code>WHERE</code></li>
					<li><code>GROUP BY</code></li>
					<li><code>HAVING</code></li>
					<li><code>ORDER BY</code></li>
					<li><code>LIMIT</code></li>
				</ol>
				<h3>Column Parsing</h3>
				<p>
					Column expressions are either plain column names or aggregate expressions such as
					<code>COUNT(*)</code>, <code>SUM(amount)</code>, and optional aliases via <code>AS</code>.
				</p>
				<div class="code-label">AST Shape (Simplified)</div>
				<pre class="code-block">{'{'}
  type: "SELECT",
  table: "users",
  columns: ["users.name", {'{'} func: "SUM", column: "orders.amount", alias: "total" {'}'}],
  joins: [...],
  where: ...,
  groupBy: ["users.name"],
  having: ...,
  orderBy: [{'{'} column: "total", direction: "DESC" {'}'}],
  limit: 5
{'}'}</pre>
			</section>

			<section>
				<h2>3. Condition Parsing and Precedence</h2>
				<p>
					Conditions use recursive descent with explicit precedence levels:
					<code>OR</code> (lowest), then <code>AND</code>, then comparison operands (highest).
				</p>
				<h3>Supported Condition Operators</h3>
				<ul>
					<li>Comparisons: <code>=</code>, <code>!=</code>, <code>&lt;</code>, <code>&gt;</code>, <code>&lt;=</code>, <code>&gt;=</code></li>
					<li><code>IS NULL</code> and <code>IS NOT NULL</code></li>
					<li><code>LIKE</code> and <code>NOT LIKE</code> (<code>%</code> and <code>_</code> wildcards)</li>
					<li><code>IN (...)</code>, <code>NOT IN (...)</code>, and subquery forms</li>
					<li>Parenthesized boolean expressions</li>
				</ul>
				<div class="code-label">Precedence Example</div>
				<pre class="code-block">WHERE A OR B AND C

Equivalent AST:
OR
|- A
`- AND
   |- B
   `- C</pre>
				<img
					src="/ast.png"
					alt="Abstract syntax tree of SQL conditions"
					class="blog-image"
				/>
			</section>

			<section>
				<h2>4. Execution Pipeline</h2>
				<p>
					Execution is implemented as a stable sequence. Each step transforms an in-memory row array and
					passes it to the next stage.
				</p>
				<h3>Clause Order During Execution</h3>
				<ol>
					<li>Load base table rows.</li>
					<li>Apply JOINs.</li>
					<li>Apply WHERE filtering.</li>
					<li>Apply GROUP BY and aggregations (if present).</li>
					<li>Project columns (when not grouped).</li>
					<li>Apply HAVING to grouped results.</li>
					<li>Apply ORDER BY sort keys.</li>
					<li>Apply LIMIT slicing.</li>
				</ol>
				<div class="code-label">Execution Skeleton</div>
				<pre class="code-block">let rows = baseTable;
rows = join(rows);
rows = where(rows);
rows = groupAndAggregate(rows);
rows = project(rows);
rows = order(rows);
rows = limit(rows);</pre>
			</section>

			<section>
				<h2>5. JOIN Semantics</h2>
				<p>
					JOIN execution uses nested iteration over left and right row sets. Merged rows include both plain
					keys and table-qualified keys like <code>users.id</code> and <code>orders.id</code> for disambiguation.
				</p>
				<ul>
					<li><strong>INNER JOIN</strong>: keep matched pairs only.</li>
					<li><strong>LEFT JOIN</strong>: keep all left rows; unmatched right fields become <code>null</code>.</li>
					<li><strong>RIGHT JOIN</strong>: keep all right rows; unmatched left fields become <code>null</code>.</li>
				</ul>
				<p>
					The <code>ON</code> clause is evaluated using the same condition engine as <code>WHERE</code>, so
					column-to-column comparisons and null semantics remain consistent.
				</p>
			</section>

			<section>
				<h2>6. GROUP BY, Aggregates, and HAVING</h2>
				<p>
					Grouping builds a hash map from group key to row bucket. Aggregate functions are then computed per
					bucket.
				</p>
				<h3>Built-in Aggregate Functions</h3>
				<ul>
					<li><code>COUNT(*)</code> and <code>COUNT(column)</code></li>
					<li><code>SUM(column)</code></li>
					<li><code>AVG(column)</code></li>
					<li><code>MIN(column)</code></li>
					<li><code>MAX(column)</code></li>
				</ul>
				<div class="code-label">Grouped Query Example</div>
				<pre class="code-block">SELECT user_id, SUM(amount) AS total
FROM orders
GROUP BY user_id
HAVING total &gt; 100
ORDER BY total DESC;</pre>
				<p>
					If a query contains aggregate columns but no explicit <code>GROUP BY</code>, execution treats the
					entire input as a single implicit group.
				</p>
			</section>

			<section>
				<h2>7. Value Resolution and Dot Notation</h2>
				<p>
					Value lookup follows a fallback strategy to support both flat and nested data:
				</p>
				<ol>
					<li>Try exact key match (<code>row[path]</code>).</li>
					<li>For dotted paths, try progressive prefix keys (useful for JOIN aliases).</li>
					<li>If needed, traverse nested objects part-by-part.</li>
				</ol>
				<div class="code-label">Examples</div>
				<pre class="code-block">address.city         // nested object traversal
users.id             // table-qualified join key
orders.amount        // table-qualified numeric value</pre>
			</section>

			<section>
				<h2>8. Type Handling Rules</h2>
				<p>
					Comparisons support mixed string/number inputs with numeric coercion when both sides parse
					cleanly as numbers. Null behavior is explicit:
				</p>
				<ul>
					<li><code>IS NULL</code> and <code>IS NOT NULL</code> use strict null/undefined checks.</li>
					<li>For <code>=</code> and <code>!=</code>, null compares directly.</li>
					<li>For range operators with null, result is false.</li>
				</ul>
				<p>
					<code>LIKE</code> patterns are converted to regex using SQL-style wildcards:
					<code>%</code> for zero-or-more chars and <code>_</code> for one char.
				</p>
			</section>

			<section>
				<h2>9. Supported and Unsupported SQL</h2>
				<h3>Supported</h3>
				<ul>
					<li><code>SELECT</code> with explicit columns or <code>*</code></li>
					<li><code>WHERE</code> with nested boolean expressions</li>
					<li><code>JOIN</code>: INNER, LEFT, RIGHT with <code>ON</code></li>
					<li><code>GROUP BY</code>, <code>HAVING</code>, and aggregates</li>
					<li><code>ORDER BY</code> and <code>LIMIT</code></li>
					<li><code>IN</code>/<code>NOT IN</code> with value lists and subqueries</li>
				</ul>
				<h3>Not Implemented</h3>
				<ul>
					<li><code>INSERT</code>, <code>UPDATE</code>, <code>DELETE</code></li>
					<li><code>UNION</code>, <code>INTERSECT</code>, window functions</li>
					<li>Expression arithmetic like <code>price * qty</code> in SELECT</li>
				</ul>
			</section>

			<section>
				<h2>10. Performance Notes</h2>
				<p>
					This parser is optimized for clarity and correctness on local JSON data. Approximate complexity:
				</p>
				<ul>
					<li>Tokenization/parsing: linear in query length.</li>
					<li>Filtering: linear in row count.</li>
					<li>Sorting: <code>O(n log n)</code>.</li>
					<li>JOINs: nested-loop by default, roughly <code>O(n*m)</code>.</li>
					<li>Grouping: linear pass plus per-group aggregate work.</li>
				</ul>
				<p>
					For very large datasets, indexing and hash-join strategies would be the next optimization step.
				</p>
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
		display: inline-block;
		padding: 0.5rem 1rem;
		background: #fafafa;
		color: black;
		border: 1px solid black;
		text-decoration: none;
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
