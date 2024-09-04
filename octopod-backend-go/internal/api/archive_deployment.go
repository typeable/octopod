package api

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gin-gonic/gin"
)

func (h *Handler) ArchiveDeploymentHandler(c *gin.Context) {
	deploymentName := c.Param("name")
	if deploymentName == "" {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Deployment name is required"})
		return
	}
	setStatusArchiving(h.Postgres, deploymentName)
	log.Printf("Status %s set to %s", deploymentName, Archiving)

	err := h.scaleResources(c, deploymentName, 0)

	if err == nil {
		log.Printf("Release %s scaled down", deploymentName)

		addArchivingAction(h.Postgres, deploymentName, "")
		log.Printf("Added action %s for %s", Archive, deploymentName)
		log.Printf("Status %s set to %s", deploymentName, Archiving)

		c.JSON(http.StatusOK, gin.H{"message": "Deployment '" + deploymentName + "' archived successfully"})
	} else {
		log.Printf("Release %s NOT scaled down", deploymentName)

		addArchivingAction(h.Postgres, deploymentName, err.Error())
		log.Printf("Added action %s for %s", Archive, deploymentName)
		log.Printf("Status %s set to %s", deploymentName, Archiving)

		c.JSON(http.StatusInternalServerError, gin.H{"error": "Archiving error"})
	}
}

func setStatusArchiving(db *sql.DB, deploymentName string) error {
	_, err := db.Exec(`
		UPDATE deployment_status
		SET status = $1, is_pending = TRUE, updated_at = $2
		WHERE deployment_id = (
			SELECT id FROM deployment WHERE name = $3
		)
	`, Archiving, time.Now(), deploymentName)
	if err != nil {
		return fmt.Errorf("failed to update status: %v", err)
	}
	return nil
}

func addArchivingAction(db *sql.DB, deploymentName string, helmError string) error {
	tx, err := db.Begin()
	if err != nil {
		return err
	}
	defer tx.Rollback()

	_, err = tx.Exec(`
        UPDATE deployment_status
        SET status = $1, is_pending = FALSE, updated_at = $2
        WHERE deployment_id = (
            SELECT id FROM deployment WHERE name = $3
        )
    `, Archived, time.Now(), deploymentName)
	if err != nil {
		return fmt.Errorf("failed to update status: %v", err)
	}

	var newActionID int
	err = tx.QueryRow(`
        INSERT INTO deployment_action (deployment_id, action, created_at, error)
        VALUES (
            (SELECT id FROM deployment WHERE name = $1),
            $2,
            $3,
			$4
        )
        RETURNING id
    `, deploymentName, Archive, time.Now(), helmError).Scan(&newActionID)
	if err != nil {
		return fmt.Errorf("failed to insert new action: %v", err)
	}

	_, err = tx.Exec(`
        INSERT INTO deployment_helm_override (deployment_action_id, version)
        SELECT $1, version
        FROM deployment_helm_override
        WHERE deployment_action_id = (
            SELECT id
            FROM deployment_action
            WHERE deployment_id = (
                SELECT id FROM deployment WHERE name = $2
            )
            ORDER BY created_at DESC
			OFFSET 1
            LIMIT 1
        )
    `, newActionID, deploymentName)
	if err != nil {
		return fmt.Errorf("failed to duplicate helm override: %v", err)
	}

	_, err = tx.Exec(`
        INSERT INTO deployment_helm_values_override (deployment_action_id, action, key, value)
        SELECT $1, action, key, value
        FROM deployment_helm_values_override
        WHERE deployment_action_id = (
            SELECT id
            FROM deployment_action
            WHERE deployment_id = (
                SELECT id FROM deployment WHERE name = $2
            )
            ORDER BY created_at DESC
			OFFSET 1
            LIMIT 1
        )
    `, newActionID, deploymentName)
	if err != nil {
		return fmt.Errorf("failed to duplicate helm values override: %v", err)
	}

	err = tx.Commit()
	if err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}
