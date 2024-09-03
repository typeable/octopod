package postgres

import (
	"database/sql"
	"log"
)

func NewDBConnection(credentials string, maxOpenConnections int, maxIdleConnection int) *sql.DB {

	db, err := sql.Open("postgres", credentials)
	if err != nil {
		log.Fatalf("Failed to connect to PostgreSQL: %v", err)
	}

	return db
}
