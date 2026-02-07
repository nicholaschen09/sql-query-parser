.PHONY: run test test-verbose test-coverage build clean test-api

run:
	go run cmd/server/main.go

test:
	go test ./...

test-verbose:
	go test -v ./...

test-coverage:
	go test -cover ./...

test-api:
	@echo "Make sure the server is running first (make run)"
	@bash test-api.sh

build:
	go build -o bin/server cmd/server/main.go

clean:
	rm -rf bin/
