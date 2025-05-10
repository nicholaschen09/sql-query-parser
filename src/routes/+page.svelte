<script lang="ts">
    import { onMount } from "svelte";
    import "../app.css";
    import type { QueryResult, QueryHistory } from "$lib/types";

    let query = "";
    let result: QueryResult | null = null;
    let history: QueryHistory[] = [];
    let loading = false;
    let queryTime: number | null = null;
    let nextId = 1;

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

    async function executeQuery() {
        if (!query.trim()) return;
        loading = true;
        queryTime = null;
        const start = performance.now();
        try {
            const response = await fetch("http://localhost:3000/api/query", {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({ query }),
            });
            result = await response.json();
            queryTime = Math.round(performance.now() - start);
            // Add to session history
            const newHistory: QueryHistory = {
                id: nextId++,
                query,
                result,
                timestamp: new Date().toISOString(),
            };
            history = [newHistory, ...history];
            saveHistory();
        } catch (error) {
            result = {
                success: false,
                error: "Failed to execute query",
            };
        }
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

    <div class="query-section">
        <textarea
            bind:value={query}
            placeholder="Enter your SQL query here..."
            rows="4"
        ></textarea>
        <div class="query-actions">
            <button on:click={executeQuery} disabled={loading}>
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

    <div class="history-section">
        <h2>Query History</h2>
        {#each history as item}
            <div class="history-item">
                <div class="query">{item.query}</div>
                <div class="timestamp">
                    {new Date(item.timestamp).toLocaleString()}
                </div>
                {#if item.result.success}
                    <div class="success">Success</div>
                {:else}
                    <div class="error">{item.result.error}</div>
                {/if}
            </div>
        {/each}
    </div>
</main>

<style>
    /* Remove global styles now in src/app.css */
    .container {
        max-width: 900px;
        margin: 0 auto;
        padding: 2rem;
    }
    h1 {
        font-size: 2.5rem;
        font-weight: 700;
        margin-bottom: 2rem;
    }
    .query-section {
        background: var(--card-bg);
        border: 1px solid var(--border);
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        padding: 2rem;
        margin-bottom: 2rem;
        color: #222;
    }
    textarea {
        width: 100%;
        padding: 1rem;
        margin-bottom: 1rem;
        font-family: "Fira Mono", monospace;
        font-size: 1.1rem;
        border: 1px solid var(--border);
        border-radius: 6px;
        background: #f5f5f5;
        color: #222;
        transition: border 0.2s;
    }
    textarea:focus {
        border: 1.5px solid var(--primary-green);
        outline: none;
    }
    button {
        padding: 0.75rem 2rem;
        background-color: var(--primary-green);
        color: #fff;
        border: none;
        border-radius: 6px;
        font-size: 1.1rem;
        font-weight: 600;
        cursor: pointer;
        transition:
            background 0.2s,
            box-shadow 0.2s;
        box-shadow: 0 2px 8px rgba(0, 200, 83, 0.08);
    }
    button:disabled {
        background-color: #bdbdbd;
        cursor: not-allowed;
    }
    button:hover:not(:disabled) {
        background-color: var(--primary-green-dark);
    }
    .result-section {
        background: var(--card-bg);
        border: 1px solid var(--border);
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        padding: 2rem;
        margin-bottom: 2rem;
        color: #222;
    }
    table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 1rem;
        background: var(--card-bg);
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 1px 4px rgba(0, 0, 0, 0.04);
    }
    th,
    td {
        border: 1px solid var(--border);
        padding: 0.75rem 1rem;
        text-align: left;
    }
    th {
        background: #f5f5f5;
        color: var(--primary-green-dark);
        font-weight: 700;
    }
    tr:nth-child(even) td {
        background: #fafafa;
    }
    .error {
        color: var(--error-text);
        background: var(--error-bg);
        padding: 1rem;
        border-radius: 6px;
        margin-top: 1rem;
        font-weight: 500;
    }
    .history-section {
        margin-top: 2rem;
    }
    .history-item {
        border: 1px solid var(--border);
        background: var(--card-bg);
        border-radius: 8px;
        padding: 1rem 1.5rem;
        margin-bottom: 1.2rem;
        box-shadow: 0 1px 4px rgba(0, 0, 0, 0.03);
        color: #222;
    }
    .history-item .query {
        font-family: "Fira Mono", monospace;
        margin-bottom: 0.5rem;
        color: var(--primary-green-dark);
        font-size: 1.05rem;
    }
    .history-item .timestamp {
        color: var(--text-light);
        font-size: 0.95rem;
        margin-bottom: 0.3rem;
    }
    .history-item .success {
        color: var(--success-text);
        font-weight: 600;
    }
    .history-item .error {
        color: var(--error-text);
        background: var(--error-bg);
        border-radius: 4px;
        padding: 0.5rem 1rem;
        margin-top: 0.5rem;
        font-weight: 500;
    }
    .query-actions {
        display: flex;
        gap: 1rem;
        margin-bottom: 0.5rem;
    }
    .clear-btn {
        background: #eee;
        color: #222;
        border: none;
        border-radius: 6px;
        font-size: 1rem;
        font-weight: 500;
        padding: 0.75rem 1.5rem;
        cursor: pointer;
        transition: background 0.2s;
    }
    .clear-btn:disabled {
        background: #ccc;
        color: #888;
        cursor: not-allowed;
    }
    .clear-btn:hover:not(:disabled) {
        background: #e0e0e0;
    }
    .spinner {
        display: inline-block;
        width: 1em;
        height: 1em;
        border: 2.5px solid #fff;
        border-top: 2.5px solid #00c853;
        border-radius: 50%;
        animation: spin 0.7s linear infinite;
        margin-right: 0.5em;
        vertical-align: middle;
    }
    @keyframes spin {
        0% {
            transform: rotate(0deg);
        }
        100% {
            transform: rotate(360deg);
        }
    }
    .result-meta {
        color: #222;
        font-size: 1.05rem;
        margin-bottom: 0.5rem;
    }
</style>
