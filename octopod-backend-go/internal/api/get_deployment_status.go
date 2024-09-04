package api

import (
	"database/sql"
	"fmt"
	"net/http"

	"github.com/gin-gonic/gin"
)

func (h *Handler) GetDeploymentStatusHandler(c *gin.Context) {
	deploymentName := c.Param("name")

	status, err := getDeploymentStatus(h.Postgres, deploymentName)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.JSON(http.StatusOK, status)
}

func getDeploymentStatus(db *sql.DB, deploymentName string) (*FullStatus, error) {
	query := `
        SELECT status, is_pending
        FROM deployment_status
        WHERE deployment_id = (
            SELECT id FROM deployment WHERE name = $1
        )
    `

	row := db.QueryRow(query, deploymentName)

	var status FullStatus
	err := row.Scan(&status.Status, &status.Pending)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, fmt.Errorf("deployment not found: %v", deploymentName)
		}
		return nil, err
	}

	return &status, nil
}
