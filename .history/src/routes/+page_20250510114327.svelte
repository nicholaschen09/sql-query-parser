<script lang="ts">
    import { onMount } from 'svelte';
    import type { QueryResult, QueryHistory } from '$lib/types';

    let query = '';
    let result: QueryResult | null = null;
    let history: QueryHistory[] = [];
    let loading = false;

    async function executeQuery() {
        if (!query.trim()) return;
        
        loading = true;
        try {
            const response = await fetch('http://localhost:3000/api/query', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ query })
            });
            result = await response.json();
            await loadHistory();
        } catch (error) {
            result = {
                success: false,
                error: 'Failed to execute query'
            };
        }
        loading = false;
    }

    async function loadHistory() {
        try {
            const response = await fetch('http://localhost:3000/api/history');
            history = await response.json();
        } catch (error) {
            console.error('Failed to load history:', error);
        }
    }

    onMount(loadHistory);
</script>

<main class="container">
    <h1>SQL Query Parser</h1>
    
    <div class="query-section">
        <textarea
            bind:value={query}
            placeholder="Enter your SQL query here..."
            rows="4"
        ></textarea>
        <button on:click={executeQuery} disabled={loading}>
            {loading ? 'Executing...' : 'Execute Query'}
        </button>
    </div>

    {#if result}
        <div class="result-section">
            <h2>Result</h2>
            {#if result.success}
                {#if result.data && result.data.length > 0}
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
                <div class="timestamp">{new Date(item.timestamp).toLocaleString()}</div>
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
    .container {
        max-width: 1200px;
        margin: 0 auto;
        padding: 2rem;
    }

    .query-section {
        margin-bottom: 2rem;
    }

    textarea {
        width: 100%;
        padding: 0.5rem;
        margin-bottom: 1rem;
        font-family: monospace;
    }

    button {
        padding: 0.5rem 1rem;
        background-color: #4CAF50;
        color: white;
        border: none;
        border-radius: 4px;
        cursor: pointer;
    }

    button:disabled {
        background-color: #cccccc;
    }

    .result-section {
        margin-bottom: 2rem;
    }

    table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 1rem;
    }

    th, td {
        border: 1px solid #ddd;
        padding: 0.5rem;
        text-align: left;
    }

    th {
        background-color: #f5f5f5;
    }

    .error {
        color: red;
        padding: 1rem;
        background-color: #ffebee;
        border-radius: 4px;
    }

    .history-section {
        margin-top: 2rem;
    }

    .history-item {
        border: 1px solid #ddd;
        padding: 1rem;
        margin-bottom: 1rem;
        border-radius: 4px;
    }

    .history-item .query {
        font-family: monospace;
        margin-bottom: 0.5rem;
    }

    .history-item .timestamp {
        color: #666;
        font-size: 0.9rem;
    }

    .history-item .success {
        color: green;
    }
</style>
