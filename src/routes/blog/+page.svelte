<div class="page-wrapper">
	<main class="container">
		<a class="back-button" href="/">← Back</a>

		<h1>How the SQL Query Parser Works</h1>

		<article class="blog-content">
			<p class="intro">
				This project uses a <strong>TypeScript-only</strong> SQL parser that transforms SQL queries into
				executable operations on JSON data directly in the browser.
			</p>
			<img
				src="/sql-string.png"
				alt="Example SQL query selecting data from a table with results shown"
				class="blog-image"
			/>

			<section>
				<h2>Architecture Overview</h2>
				<p>
					The parser follows a three-stage pipeline: <strong>tokenization</strong>, <strong>parsing</strong>,
					and <strong>execution</strong>. SQL text goes in, an AST is produced, and filtered/projected rows
					come out.
				</p>
				<img
					src="/parser.png"
					alt="Parser architecture: SQL input flows through tokenization, parsing, and execution"
					class="blog-image"
				/>
				<div class="code-label">TypeScript</div>
				<pre class="code-block">class SQLParser {'{'}
  constructor(private tables: Record&lt;string, Row[]&gt;) {'{'}{'}'}
  parse(query: string): SelectQuery {'{'} ... {'}'}
  execute(ast: SelectQuery): Row[] {'{'} ... {'}'}
{'}'}</pre>
			</section>

			<section>
				<h2>1. Tokenization</h2>
				<p>
					Tokenization normalizes punctuation, splits by whitespace, and removes trailing semicolons.
				</p>
				<pre class="code-block">private tokenize(query: string): string[] {'{'}
  return query
    .replace(/[(),]/g, (m) =&gt; ` ${'{'}m{'}'} `)
    .replace(/;$/, '')
    .trim()
    .split(/\s+/)
    .filter(Boolean);
{'}'}</pre>
				<img
					src="/tokenization.png"
					alt="Tokenization: query text broken into SQL tokens"
					class="blog-image"
				/>
			</section>

			<section>
				<h2>2. Parsing</h2>
				<p>
					The parser validates <code>SELECT</code>, extracts columns and table name, then parses optional
					clauses like <code>WHERE</code>, <code>JOIN</code>, <code>GROUP BY</code>, <code>ORDER BY</code>,
					and <code>LIMIT</code>.
				</p>
				<div class="code-label">TypeScript</div>
				<pre class="code-block">const parser = new SQLParser(tables);
const ast = parser.parse("SELECT state, pop FROM table WHERE pop &gt; 5000");

// AST (simplified)
// {'{'} type: "SELECT", columns: ["state", "pop"], table: "table", where: ... {'}'}</pre>
			</section>

			<section>
				<h2>3. Recursive WHERE Parsing</h2>
				<p>
					WHERE clauses are parsed with recursive descent so precedence is correct:
					<code>OR</code> (lowest), then <code>AND</code>, then comparison expressions.
				</p>
				<pre class="code-block">private parseConditionRecursive(tokens: string[], idx: number) {'{'}
  let [left, next] = this.parseAnd(tokens, idx);
  while (tokens[next]?.toUpperCase() === "OR") {'{'}
    const [right, afterRight] = this.parseAnd(tokens, next + 1);
    left = {'{'} left, operator: "OR", right {'}'};
    next = afterRight;
  {'}'}
  return [left, next] as const;
{'}'}</pre>
				<img
					src="/ast.png"
					alt="Abstract syntax tree of SQL conditions"
					class="blog-image"
				/>
			</section>

			<section>
				<h2>4. Execution</h2>
				<p>
					Execution runs in predictable steps: filter rows, project selected columns, apply ordering and
					limit, then return the result set.
				</p>
				<div class="code-label">TypeScript</div>
				<pre class="code-block">const rows = parser.execute(ast);
// 1) WHERE filters rows
// 2) SELECT projects columns
// 3) ORDER BY / LIMIT finalize output</pre>
			</section>

			<section>
				<h2>Supported Features</h2>
				<ul>
					<li>SELECT with specific columns or *</li>
					<li>WHERE with AND, OR, and parentheses</li>
					<li>Comparison operators, LIKE, IN, and NULL checks</li>
					<li>JOINs, GROUP BY, HAVING, ORDER BY, and LIMIT</li>
					<li>Aggregations: COUNT, SUM, AVG, MIN, MAX</li>
					<li>Nested object access via dot notation</li>
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
