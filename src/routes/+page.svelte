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
    let data: Row[] = [];
    let jsonError: string | null = null;
    let isDragging = false;
    let previewData: string = "";
    let inputMode: "file" | "raw" = "file";

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
            data = JSON.parse(input);
            jsonError = null;
            previewData = JSON.stringify(data.slice(0, 3), null, 2);
        } catch (err) {
            jsonError = "Invalid JSON input.";
            data = [];
            previewData = "";
        }
    }

    function handleFileChange(event: Event) {
        const file = (event.target as HTMLInputElement).files?.[0];
        if (file) {
            processFile(file);
        }
    }

    function processFile(file: File) {
        const reader = new FileReader();
        reader.onload = (e) => {
            processJsonInput(e.target?.result as string);
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

    function executeQuery() {
        if (!query.trim()) return;
        if (!data.length) {
            result = { success: false, error: "No JSON data loaded." };
            return;
        }
        loading = true;
        queryTime = null;
        const start = performance.now();
        try {
            const parser = new SQLParser(data);
            const parsed = parser.parse(query);
            const res = parser.execute(parsed);
            result = { success: true, data: res };
            queryTime = Math.round(performance.now() - start);
        } catch (error: any) {
            result = {
                success: false,
                error: error.message || "Failed to execute query",
            };
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

    onMount(() => {
        loadHistoryFromSession();
    });
</script>

<main class="container">
    <h1>SQL Query Parser</h1>

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
            <div class="preview-section">
                <h3>Data Preview</h3>
                <pre class="json-preview">{previewData}</pre>
                <p class="preview-note">
                    Showing first 3 records of {data.length} total records
                </p>
            </div>
        {/if}
    </div>

    <div class="query-section">
        <h2>SQL Query</h2>
        <textarea
            bind:value={query}
            placeholder="Enter your SQL query here..."
            rows="4"
        ></textarea>
        <div class="query-actions">
            <button on:click={executeQuery} disabled={loading || !data.length}>
                {#if loading}
                    <span class="spinner"></span> Executing...
                {:else}
                    Execute Query
                {/if}
            </button>
            <button class="clear-btn" on:click={clearQuery} disabled={loading}
                >Clear Query</button
            >
            <button
                class="clear-btn"
                on:click={clearHistory}
                disabled={loading || history.length === 0}>Clear History</button
            >
        </div>
    </div>

    {#if result}
        <div class="result-section">
            <h2>Result</h2>
            {#if result.success}
                {#if result.data && result.data.length > 0}
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
                {:else}
                    <p>No results found</p>
                {/if}
            {:else}
                <div class="error">
                    {result.error}
                </div>
            {/if}
        </div>
    {/if}

    {#if history.length > 0}
        <div class="history-section">
            <h2>Query History</h2>
            {#each history as item}
                <div class="history-item">
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
</main>

<style>
    .json-input-section {
        margin-bottom: 2.2rem;
    }

    .upload-zone {
        border: 2px dashed var(--border);
        border-radius: 12px;
        padding: 2rem;
        text-align: center;
        background: rgba(255, 255, 255, 0.05);
        transition: all 0.3s ease;
        cursor: pointer;
    }

    .upload-zone.dragging {
        border-color: var(--primary-green);
        background: rgba(0, 200, 83, 0.1);
    }

    .upload-content {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 1rem;
    }

    .upload-icon {
        color: var(--primary-green);
        margin-bottom: 1rem;
    }

    .upload-button {
        background: var(--primary-green);
        color: white;
        padding: 0.75rem 1.5rem;
        border-radius: 6px;
        cursor: pointer;
        font-weight: 600;
        transition: background 0.2s;
    }

    .upload-button:hover {
        background: var(--primary-green-dark);
    }

    .hidden {
        display: none;
    }

    .preview-section {
        margin-top: 1.5rem;
        background: var(--card-bg);
        border-radius: 8px;
        padding: 1.5rem;
        color: #222;
    }

    .json-preview {
        background: #f5f5f5;
        padding: 1rem;
        border-radius: 6px;
        overflow-x: auto;
        font-family: "Fira Mono", monospace;
        font-size: 0.9rem;
        margin: 1rem 0;
    }

    .preview-note {
        color: #666;
        font-size: 0.9rem;
        margin: 0;
    }

    .table-container {
        overflow-x: auto;
        margin-top: 1rem;
    }

    h2 {
        color: var(--text);
        margin-bottom: 1rem;
        font-size: 1.5rem;
    }

    h3 {
        color: var(--text);
        margin: 0;
        font-size: 1.2rem;
    }

    p {
        margin: 0.5rem 0;
        color: var(--text-light);
    }

    .input-mode-toggle {
        display: flex;
        gap: 1rem;
        margin-bottom: 1.5rem;
    }

    .toggle-btn {
        flex: 1;
        background: rgba(255, 255, 255, 0.1);
        color: var(--text);
        border: 1px solid var(--border);
        padding: 0.75rem;
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s;
    }

    .toggle-btn.active {
        background: var(--primary-green);
        border-color: var(--primary-green);
    }

    .toggle-btn:hover:not(.active) {
        background: rgba(255, 255, 255, 0.15);
    }

    .raw-json-section {
        background: var(--card-bg);
        border-radius: 8px;
        padding: 1.5rem;
        margin-bottom: 1.5rem;
    }

    .raw-json-section textarea {
        width: 100%;
        min-height: 200px;
        font-family: "Fira Mono", monospace;
        font-size: 0.9rem;
        line-height: 1.5;
        background: #f5f5f5;
        border: 1px solid var(--border);
        border-radius: 6px;
        padding: 1rem;
        color: #222;
        resize: vertical;
    }

    .raw-json-section textarea:focus {
        border-color: var(--primary-green);
        outline: none;
    }

    .query-section {
        background: var(--card-bg);
        border: 1px solid var(--border);
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        padding: 1.2rem 1.5rem 2rem;
        margin-bottom: 2rem;
        color: #222;
        display: flex;
        flex-direction: column;
        align-items: stretch;
    }

    .query-section h2 {
        margin-top: 0;
        margin-bottom: 0.5rem;
    }

    .query-section textarea {
        margin-bottom: 1.5rem;
        margin-top: -1.5rem;
        border: 1px solid #bdbdbd;
        border-radius: 8px;
        font-size: 1.15rem;
        padding: 1.1rem;
        background: #f7f7f7;
        color: #222;
        transition: border 0.2s;
    }
    .query-section textarea:focus {
        border: 1.5px solid var(--primary-green);
        outline: none;
    }

    .query-actions {
        display: flex;
        gap: 0.7rem;
        margin-bottom: 0;
        justify-content: flex-start;
    }
    button,
    .clear-btn {
        padding: 0.65rem 1.5rem;
        background-color: var(--primary-green);
        color: #fff;
        border: none;
        border-radius: 6px;
        font-size: 1.05rem;
        font-weight: 600;
        cursor: pointer;
        transition:
            background 0.2s,
            box-shadow 0.2s;
        box-shadow: 0 1px 4px rgba(0, 200, 83, 0.07);
        min-width: 140px;
    }
    button:disabled,
    .clear-btn:disabled {
        background-color: #e0e0e0;
        color: #aaa;
        cursor: not-allowed;
    }
    button:hover:not(:disabled),
    .clear-btn:hover:not(:disabled) {
        background-color: var(--primary-green-dark);
    }
    .clear-btn {
        background: #e0e0e0;
        color: #222;
        border: none;
        border-radius: 6px;
        font-size: 1.05rem;
        font-weight: 500;
        transition: background 0.2s;
    }
    .clear-btn:disabled {
        background: #f3f3f3;
        color: #bbb;
    }
</style>
