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
	Query  string                        `json:"query"`
	Data   []parser.Row                  `json:"data,omitempty"`
	Tables map[string][]parser.Row       `json:"tables,omitempty"`
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
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Content-Type", "application/json")

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
		json.NewEncoder(w).Encode(ExecuteResponse{Success: false, Error: &errorMsg})
		return
	}

	log.Printf("Received query: %s", req.Query)

	// Build parser from tables or data
	var sqlParser *parser.SQLParser
	if req.Tables != nil && len(req.Tables) > 0 {
		sqlParser = parser.NewSQLParser(req.Tables)
	} else if req.Data != nil {
		sqlParser = parser.NewSQLParserFromData(req.Data)
	} else {
		errorMsg := "No data or tables provided"
		json.NewEncoder(w).Encode(ExecuteResponse{Success: false, Error: &errorMsg})
		return
	}

	parsedQuery, err := sqlParser.Parse(req.Query)
	if err != nil {
		log.Printf("Parse error: %v", err)
		errorMsg := err.Error()
		json.NewEncoder(w).Encode(ExecuteResponse{Success: false, Error: &errorMsg})
		return
	}

	results, err := sqlParser.Execute(parsedQuery)
	if err != nil {
		log.Printf("Execute error: %v", err)
		errorMsg := err.Error()
		json.NewEncoder(w).Encode(ExecuteResponse{Success: false, Error: &errorMsg})
		return
	}

	log.Printf("Query executed successfully, returned %d rows", len(results))
	json.NewEncoder(w).Encode(ExecuteResponse{Success: true, Data: results})
}
