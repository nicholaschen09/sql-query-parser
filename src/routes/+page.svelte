<script lang="ts">
    import { onMount } from "svelte";
    import "../app.css";
    import { SQLParser } from "$lib/parser/sql-parser";
    import type { QueryResult, QueryHistory, Row } from "$lib/types/types";

    let query = "";
    let result: QueryResult | null = null;
    let history: QueryHistory[] = [];
    let loading = false;
    let queryTime: number | null = null;
    let nextId = 1;
    let jsonInput = "";
    let tables: Record<string, Row[]> = {};
    let currentTable: string | null = null;
    let jsonError: string | null = null;
    let isDragging = false;
    let previewData: string = "";
    let inputMode: "file" | "raw" = "file";
    let copyMessage = "";
    let copyTimeout: ReturnType<typeof setTimeout> | null = null;
    let copySqlMessage = "";
    let copySqlTimeout: ReturnType<typeof setTimeout> | null = null;
    let selectedHistory: QueryHistory | null = null;
    let parserType: "typescript" | "go" = "typescript";
    let goApiUrl = "http://localhost:8080";

    // Sample data for suggestions
    const sampleJsons = [
        {
            label: "US States Population",
            value: `[
  { "state": "California", "region": "West", "pop": 39538223, "pop_male": 19453769, "pop_female": 20084454 },
  { "state": "Texas", "region": "South", "pop": 29145505, "pop_male": 14358470, "pop_female": 14787035 },
  { "state": "Florida", "region": "South", "pop": 21538187, "pop_male": 10470577, "pop_female": 11067610 }
]`,
        },
        {
            label: "World Cities",
            value: `[
  { "city": "Tokyo", "country": "Japan", "population": 37400068, "area_km2": 2191 },
  { "city": "Delhi", "country": "India", "population": 28514000, "area_km2": 1484 },
  { "city": "Shanghai", "country": "China", "population": 25582000, "area_km2": 6340 }
]`,
        },
        {
            label: "Books",
            value: `[
  { "title": "To Kill a Mockingbird", "author": "Harper Lee", "year": 1960, "genre": "Fiction" },
  { "title": "1984", "author": "George Orwell", "year": 1949, "genre": "Dystopian" },
  { "title": "The Great Gatsby", "author": "F. Scott Fitzgerald", "year": 1925, "genre": "Classic" }
]`,
        },
        // Add more samples if you want
    ];

    const sampleSqlMap: Record<string, { label: string; value: string }[]> = {
        "US States Population": [
            { label: "All States", value: "SELECT * FROM table;" },
            {
                label: "States with pop > 20M",
                value: "SELECT state, pop FROM table WHERE pop > 20000000;",
            },
            {
                label: "States in the South",
                value: "SELECT state FROM table WHERE region = 'South';",
            },
        ],
        "World Cities": [
            { label: "All Cities", value: "SELECT * FROM table;" },
            {
                label: "Cities with pop > 25M",
                value: "SELECT city, population FROM table WHERE population > 25000000;",
            },
            {
                label: "Cities in China",
                value: "SELECT city FROM table WHERE country = 'China';",
            },
        ],
        Books: [
            { label: "All Books", value: "SELECT * FROM table;" },
            {
                label: "Books before 1950",
                value: "SELECT title, year FROM table WHERE year < 1950;",
            },
            {
                label: "Fiction Books",
                value: "SELECT title FROM table WHERE genre = 'Fiction';",
            },
        ],
    };

    let sampleSqls = sampleSqlMap["US States Population"];

    function setSampleSqlsFor(label: string) {
        sampleSqls =
            sampleSqlMap[label] || sampleSqlMap["US States Population"];
    }

    function saveHistory() {
        sessionStorage.setItem("queryHistory", JSON.stringify(history));
        sessionStorage.setItem("queryHistoryNextId", String(nextId));
    }

    function loadHistoryFromSession() {
        const h = sessionStorage.getItem("queryHistory");
        const n = sessionStorage.getItem("queryHistoryNextId");
        history = h ? JSON.parse(h) : [];
        nextId = n ? parseInt(n) : 1;
    }

    function processJsonInput(input: string) {
        jsonInput = input;
        try {
            const rows = JSON.parse(input);
            if (Array.isArray(rows)) {
                addTable("RawInput", rows);
                jsonError = null;
            } else {
                jsonError = "Input is not a JSON array.";
            }
        } catch (err) {
            jsonError = "Invalid JSON input.";
        }
    }

    function handleFileChange(event: Event) {
        const files = (event.target as HTMLInputElement).files;
        if (files) {
            for (const file of Array.from(files)) {
                processFile(file);
            }
        }
    }

    function processFile(file: File) {
        const reader = new FileReader();
        reader.onload = (e) => {
            try {
                const rows = JSON.parse(e.target?.result as string);
                if (Array.isArray(rows)) {
                    addTable(file.name.replace(/\.[^/.]+$/, ""), rows);
                } else {
                    jsonError = `File ${file.name} does not contain a JSON array.`;
                }
            } catch (err) {
                jsonError = `Invalid JSON in file ${file.name}.`;
            }
        };
        reader.readAsText(file);
    }

    function handleDragOver(event: DragEvent) {
        event.preventDefault();
        isDragging = true;
    }

    function handleDragLeave() {
        isDragging = false;
    }

    function handleDrop(event: DragEvent) {
        event.preventDefault();
        isDragging = false;
        const file = event.dataTransfer?.files[0];
        if (file && file.type === "application/json") {
            processFile(file);
        } else {
            jsonError = "Please drop a valid JSON file.";
        }
    }

    async function executeQuery() {
        if (!query.trim()) return;
        if (!currentTable || !tables[currentTable]) {
            result = { success: false, error: "No JSON data loaded." };
            return;
        }
        loading = true;
        queryTime = null;
        const start = performance.now();
        
        try {
            if (parserType === "go") {
                // Use Go parser via API
                const response = await fetch(`${goApiUrl}/execute`, {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: query,
                        data: tables[currentTable],
                    }),
                });
                
                if (!response.ok) {
                    const errorText = await response.text();
                    throw new Error(`HTTP ${response.status}: ${errorText || response.statusText}`);
                }
                
                const goResult = await response.json();
                if (!goResult.success && goResult.error) {
                    throw new Error(goResult.error);
                }
                
                result = {
                    success: goResult.success,
                    data: goResult.data,
                    error: goResult.error,
                };
            } else {
                // Use TypeScript parser
                const parser = new SQLParser(tables[currentTable]);
                const parsed = parser.parse(query);
                const res = parser.execute(parsed);
                result = { success: true, data: res };
            }
            
            queryTime = Math.round(performance.now() - start);
        } catch (error: any) {
            let errorMessage = error.message || "Failed to execute query";
            
            // Provide helpful error messages for common issues
            if (errorMessage.includes("Failed to fetch") || errorMessage.includes("NetworkError")) {
                errorMessage = `Cannot connect to Go server at ${goApiUrl}. Make sure the Go server is running (cd go && go run cmd/server/main.go)`;
            } else if (errorMessage.includes("HTTP")) {
                errorMessage = `Go server error: ${errorMessage}`;
            }
            
            result = {
                success: false,
                error: errorMessage,
            };
            queryTime = null;
        }
        
        // Add to session history
        const newHistory: QueryHistory = {
            id: nextId++,
            query,
            result: result as QueryResult,
            timestamp: new Date().toISOString(),
        };
        history = [newHistory, ...history];
        saveHistory();
        loading = false;
    }

    function clearQuery() {
        query = "";
        result = null;
        queryTime = null;
    }

    function clearHistory() {
        history = [];
        nextId = 1;
        saveHistory();
    }

    function copyToClipboard(text: string) {
        navigator.clipboard.writeText(text).then(() => {
            copyMessage = "Copied!";
            if (copyTimeout) clearTimeout(copyTimeout);
            copyTimeout = setTimeout(() => (copyMessage = ""), 1200);
        });
    }

    function copySqlToClipboard() {
        navigator.clipboard.writeText(query).then(() => {
            copySqlMessage = "Copied!";
            if (copySqlTimeout) clearTimeout(copySqlTimeout);
            copySqlTimeout = setTimeout(() => (copySqlMessage = ""), 1200);
        });
    }

    // Helper to add a new table
    function addTable(name: string, rows: Row[]) {
        tables = { ...tables, [name]: rows };
        if (!currentTable) currentTable = name;
    }

    // Helper to remove a table
    function removeTable(name: string) {
        const { [name]: _, ...rest } = tables;
        tables = rest;
        if (currentTable === name) {
            currentTable = Object.keys(tables)[0] || null;
        }
    }

    // Preview logic for current table
    $: previewData =
        currentTable && tables[currentTable]
            ? JSON.stringify(tables[currentTable], null, 2)
            : "";

    // Export helpers
    function exportResultAsJSON() {
        if (result && result.data) {
            const blob = new Blob([JSON.stringify(result.data, null, 2)], {
                type: "application/json",
            });
            const url = URL.createObjectURL(blob);
            const a = document.createElement("a");
            a.href = url;
            a.download = "query_result.json";
            a.click();
            URL.revokeObjectURL(url);
        }
    }

    function exportResultAsCSV() {
        if (result && result.data && result.data.length > 0) {
            const headers = Object.keys(result.data[0]);
            const csvRows = [headers.join(",")];
            for (const row of result.data) {
                csvRows.push(
                    headers.map((h) => JSON.stringify(row[h] ?? "")).join(","),
                );
            }
            const csv = csvRows.join("\n");
            const blob = new Blob([csv], { type: "text/csv" });
            const url = URL.createObjectURL(blob);
            const a = document.createElement("a");
            a.href = url;
            a.download = "query_result.csv";
            a.click();
            URL.revokeObjectURL(url);
        }
    }

    // Excel export (SheetJS)
    async function exportResultAsExcel() {
        if (result && result.data && result.data.length > 0) {
            try {
                const XLSX = await import("xlsx");
                const ws = XLSX.utils.json_to_sheet(result.data);
                const wb = XLSX.utils.book_new();
                XLSX.utils.book_append_sheet(wb, ws, "Results");
                const wbout = XLSX.write(wb, {
                    bookType: "xlsx",
                    type: "array",
                });
                const blob = new Blob([wbout], {
                    type: "application/octet-stream",
                });
                const url = URL.createObjectURL(blob);
                const a = document.createElement("a");
                a.href = url;
                a.download = "query_result.xlsx";
                a.click();
                URL.revokeObjectURL(url);
            } catch (e) {
                alert("Excel export requires SheetJS (xlsx) to be installed.");
            }
        }
    }

    onMount(() => {
        loadHistoryFromSession();
    });
</script>

<div class="page-wrapper">
<main class="container">
    <h1>SQL Query Parser</h1>
    <a href="/blog" class="blog-link">How it works</a>

    <div class="json-input-section">
        <div class="input-mode-toggle">
            <button
                class="toggle-btn"
                class:active={inputMode === "file"}
                on:click={() => (inputMode = "file")}
            >
                Upload File
            </button>
            <button
                class="toggle-btn"
                class:active={inputMode === "raw"}
                on:click={() => (inputMode = "raw")}
            >
                Raw JSON
            </button>
        </div>

        <!-- Sample JSON suggestions -->
        <div class="sample-suggestions">
            <span>Try sample JSON:</span>
            {#each sampleJsons as sample}
                <button
                    type="button"
                    on:click={() => {
                        jsonInput = sample.value;
                        processJsonInput(sample.value);
                        setSampleSqlsFor(sample.label);
                    }}
                >
                    {sample.label}
                </button>
            {/each}
        </div>

        {#if inputMode === "file"}
            <div
                class="upload-zone"
                class:dragging={isDragging}
                on:dragover={handleDragOver}
                on:dragleave={handleDragLeave}
                on:drop={handleDrop}
            >
                <div class="upload-content">
                    <svg
                        class="upload-icon"
                        viewBox="0 0 24 24"
                        width="48"
                        height="48"
                    >
                        <path
                            fill="currentColor"
                            d="M19.35 10.04C18.67 6.59 15.64 4 12 4 9.11 4 6.6 5.64 5.35 8.04 2.34 8.36 0 10.91 0 14c0 3.31 2.69 6 6 6h13c2.76 0 5-2.24 5-5 0-2.64-2.05-4.78-4.65-4.96zM14 13v4h-4v-4H7l5-5 5 5h-3z"
                        />
                    </svg>
                    <h3>Drop your JSON file here</h3>
                    <p>or</p>
                    <label class="upload-button">
                        Choose File
                        <input
                            type="file"
                            accept=".json,application/json"
                            on:change={handleFileChange}
                            class="hidden"
                        />
                    </label>
                </div>
            </div>
        {:else}
            <div class="raw-json-section">
                <h2>JSON Input</h2>
                <textarea
                    bind:value={jsonInput}
                    placeholder="Paste your JSON data here..."
                    rows="8"
                    on:input={() => processJsonInput(jsonInput)}
                ></textarea>
            </div>
        {/if}

        {#if jsonError}
            <div class="error">{jsonError}</div>
        {/if}

        {#if previewData}
            <div class="preview-section" style="position:relative;">
                <h2>Preview</h2>
                <pre class="json-preview">{previewData}</pre>
                <div
                    style="display:flex;justify-content:space-between;align-items:center;margin-top:0.5rem;"
                >
                    <p class="preview-note" style="margin:0;">
                        Showing {(currentTable &&
                            tables[currentTable]?.length) ||
                            0} total record{(currentTable &&
                            tables[currentTable]?.length) === 1
                            ? ""
                            : "s"}
                    </p>
                    <button
                        class="clear-btn"
                        style="margin-left:1rem;"
                        on:click={() => copyToClipboard(previewData)}
                    >
                        Copy to Clipboard
                    </button>
                </div>
                {#if copyMessage}
                    <div>
                        {copyMessage}
                    </div>
                {/if}
            </div>
        {/if}
    </div>

    <div class="query-section">
        <!-- Sample SQL suggestions -->
        <div class="sample-suggestions">
            <span>Try sample SQL:</span>
            {#each sampleSqls as sample}
                <button type="button" on:click={() => (query = sample.value)}>
                    {sample.label}
                </button>
            {/each}
        </div>
        <h2>SQL Query</h2>
        <div style="margin-bottom: 0.5rem;">
            <label style="display: flex; align-items: center; gap: 0.5rem;">
                <span>Parser:</span>
                <select bind:value={parserType}>
                    <option value="typescript">TypeScript (Client-side)</option>
                    <option value="go">Go (Server-side)</option>
                </select>
            </label>
            {#if parserType === "go"}
                <div style="margin-top: 0.3rem;">
                    <label style="display: flex; align-items: center; gap: 0.5rem;">
                        <span>Go API URL:</span>
                        <input
                            type="text"
                            bind:value={goApiUrl}
                            placeholder="http://localhost:8080"
                            style="flex: 1; max-width: 300px;"
                        />
                    </label>
                </div>
            {/if}
        </div>
        <textarea
            bind:value={query}
            placeholder="Enter your SQL query here..."
            rows="4"
            style="width:100%;"
        ></textarea>
        <div
            style="display:flex;align-items:center;justify-content:space-between;margin-top:0.3rem;gap:1rem;"
        >
            <div class="query-actions" style="margin:0;">
                <button
                    on:click={executeQuery}
                    disabled={loading || !currentTable}
                >
                    {#if loading}
                        <span class="spinner"></span> Executing...
                    {:else}
                        Execute Query
                    {/if}
                </button>
                <button
                    class="clear-btn"
                    on:click={clearQuery}
                    disabled={loading}>Clear Query</button
                >
                <button
                    class="clear-btn"
                    on:click={clearHistory}
                    disabled={loading || history.length === 0}
                    >Clear History</button
                >
            </div>
            <button class="clear-btn" on:click={copySqlToClipboard}>
                Copy to Clipboard
            </button>
        </div>
        {#if copySqlMessage}
            <div style="text-align:right;">
                {copySqlMessage}
            </div>
        {/if}
    </div>

    {#if result}
        <div class="result-section">
            <h2>Result</h2>
            {#if result && result.success}
                {#if result.data && result.data.length > 0}
                    <div style="display:flex;gap:0.7rem;margin-bottom:0.7rem;">
                        <button class="clear-btn" on:click={exportResultAsJSON}
                            >Export JSON</button
                        >
                        <button class="clear-btn" on:click={exportResultAsCSV}
                            >Export CSV</button
                        >
                        <button class="clear-btn" on:click={exportResultAsExcel}
                            >Export Excel</button
                        >
                    </div>
                    <button
                        class="clear-btn"
                        style="float:right;margin-top:-8px;margin-bottom:8px"
                        on:click={() =>
                            result &&
                            copyToClipboard(
                                JSON.stringify(result.data, null, 2),
                            )}
                    >
                        Copy to Clipboard
                    </button>
                    <div class="result-meta">
                        <span
                            >{result.data.length} row{result.data.length === 1
                                ? ""
                                : "s"} returned</span
                        >
                        {#if queryTime !== null}
                            <span> | Query time: {queryTime} ms</span>
                        {/if}
                    </div>
                    <div class="table-container">
                        <table>
                            <thead>
                                <tr>
                                    {#each Object.keys(result.data[0]) as header}
                                        <th>{header}</th>
                                    {/each}
                                </tr>
                            </thead>
                            <tbody>
                                {#each result.data as row}
                                    <tr>
                                        {#each Object.values(row) as value}
                                            <td>{value}</td>
                                        {/each}
                                    </tr>
                                {/each}
                            </tbody>
                        </table>
                    </div>
                    {#if copyMessage}
                        <div style="color:#067800;font-weight:600;">
                            {copyMessage}
                        </div>
                    {/if}
                {:else}
                    <p>No results found</p>
                {/if}
            {:else if result && result.error}
                <div class="error">
                    {result.error}
                </div>
            {/if}
        </div>
    {/if}

    {#if history.length > 0}
        <div class="history-section">
            <h2>Query History ({history.length})</h2>
            {#each history as item}
                <div
                    class="history-item"
                    style="cursor:pointer;"
                    on:click={() => (selectedHistory = item)}
                >
                    <div class="query">{item.query}</div>
                    <div class="timestamp">
                        {new Date(item.timestamp).toLocaleString()}
                    </div>
                    {#if item.result.success}
                        <div class="success">
                            {item.result.data?.length || 0} rows returned
                        </div>
                    {:else}
                        <div class="error">{item.result.error}</div>
                    {/if}
                </div>
            {/each}
        </div>
    {/if}

    {#if selectedHistory}
        <div
            class="modal-backdrop"
            style="position:fixed;top:0;left:0;width:100vw;height:100vh;background:rgba(0,0,0,0.5);z-index:1000;display:flex;align-items:center;justify-content:center;"
        >
            <div
                class="modal-content"
                style="background:white;padding:1.2rem 2.5rem 2rem 2.5rem;border:1px solid black;max-width:600px;width:90vw;position:relative;overflow:auto;"
            >
                <button
                    on:click={() => (selectedHistory = null)}
                    style="position:absolute;top:1rem;right:1rem;font-size:1.5rem;background:none;border:none;cursor:pointer;color:black;"
                    >&times;</button
                >
                <h2 style="margin-top:0;">Query Details</h2>
                <div style="margin-bottom:1.2rem;">
                    <strong>SQL Query:</strong><br
                    /><span
                        style="font-family:'Courier New',monospace;"
                        >{selectedHistory.query}</span
                    >
                </div>
                <div style="margin-bottom:1.2rem;">
                    <strong>Timestamp:</strong>
                    <span
                        >{new Date(
                            selectedHistory.timestamp,
                        ).toLocaleString()}</span
                    >
                </div>
                {#if selectedHistory.result.success}
                    <div style="margin-bottom:1.2rem;">
                        <strong>Rows Returned:</strong>
                        <span
                            >{selectedHistory.result.data?.length || 0}</span
                        >
                    </div>
                    <div style="margin-bottom:1.2rem;">
                        <strong>Result Data:</strong>
                        <pre
                            style="background:white;padding:1rem;border:1px solid black;max-height:250px;overflow:auto;font-size:0.9rem;">{JSON.stringify(
                                selectedHistory.result.data,
                                null,
                                2,
                            )}</pre>
                    </div>
                {:else}
                    <div class="error" style="margin-bottom:1.2rem;">
                        {selectedHistory.result.error}
                    </div>
                {/if}
            </div>
        </div>
    {/if}

    {#if Object.keys(tables).length > 0}
        <div class="table-list-section">
            <h3>Loaded Tables</h3>
            <ul
                style="list-style:none;padding:0;display:flex;gap:1rem;flex-wrap:wrap;"
            >
                {#each Object.keys(tables) as tableName}
                    <li style="display:flex;align-items:center;gap:0.3rem;">
                        <button
                            class="clear-btn"
                            class:active={currentTable === tableName}
                            on:click={() => (currentTable = tableName)}
                        >
                            {tableName}
                        </button>
                        <button
                            class="clear-btn"
                            style="font-size:0.9rem;padding:0.3rem 0.7rem;"
                            on:click={() => removeTable(tableName)}
                            title="Remove table"
                        >
                            ✕
                        </button>
                    </li>
                {/each}
            </ul>
        </div>
    {/if}
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