package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"

	"sql-query-parser-go/internal/parser"
)

type ExecuteRequest struct {
	Query string       `json:"query"`
	Data  []parser.Row `json:"data"`
}

type ExecuteResponse struct {
	Success bool         `json:"success"`
	Data    []parser.Row `json:"data,omitempty"`
	Error   *string      `json:"error,omitempty"`
}

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
	}

	http.HandleFunc("/execute", handleExecute)
	http.HandleFunc("/health", handleHealth)

	log.Printf("Go SQL Parser server starting on port %s", port)
	log.Fatal(http.ListenAndServe(":"+port, nil))
}

func handleHealth(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}

func handleExecute(w http.ResponseWriter, r *http.Request) {
	// Set CORS headers first
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Content-Type", "application/json")

	// Handle preflight OPTIONS request
	if r.Method == http.MethodOptions {
		w.WriteHeader(http.StatusOK)
		return
	}

	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req ExecuteRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		log.Printf("Error decoding request: %v", err)
		errorMsg := fmt.Sprintf("Invalid request: %v", err)
		response := ExecuteResponse{
			Success: false,
			Error:   &errorMsg,
		}
		json.NewEncoder(w).Encode(response)
		return
	}

	log.Printf("Received query: %s", req.Query)
	log.Printf("Data rows: %d", len(req.Data))

	// Create parser and execute query
	sqlParser := parser.NewSQLParser(req.Data)
	parsedQuery, err := sqlParser.Parse(req.Query)
	if err != nil {
		log.Printf("Parse error: %v", err)
		errorMsg := err.Error()
		response := ExecuteResponse{
			Success: false,
			Error:   &errorMsg,
		}
		json.NewEncoder(w).Encode(response)
		return
	}

	results, err := sqlParser.Execute(parsedQuery)
	if err != nil {
		log.Printf("Execute error: %v", err)
		errorMsg := err.Error()
		response := ExecuteResponse{
			Success: false,
			Error:   &errorMsg,
		}
		json.NewEncoder(w).Encode(response)
		return
	}

	log.Printf("Query executed successfully, returned %d rows", len(results))

	response := ExecuteResponse{
		Success: true,
		Data:    results,
	}

	if err := json.NewEncoder(w).Encode(response); err != nil {
		log.Printf("Error encoding response: %v", err)
	}
}
