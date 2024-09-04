package api

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gin-gonic/gin"
)

func (h *Handler) RestoreDeploymentHandler(c *gin.Context) {

	deploymentName := c.Param("name")
	if deploymentName == "" {
		log.Printf("Error: Deployment name is required")
		c.JSON(http.StatusBadRequest, ginError("Invalid request payload"))
		return
	}
	log.Println("Starting deployment restoring")
	log.SetPrefix(deploymentName)

	deploymentId, deploymentActionId, err := saveRestoreAction(h.Postgres, deploymentName)
	if err != nil {
		log.Printf("Error setting status to %s: %v", Restoring, err)
		c.JSON(http.StatusInternalServerError, ginError("Something went wrong"))
		return
	}
	log.Printf("Status %s set to %s", deploymentName, Restoring)
	c.JSON(http.StatusOK, ginHelmProcess())

	go func() {
		log.Printf("Scaling resources for deployment %s to 1", deploymentName)
		err = h.scaleResources(c, deploymentName, 1)
		if err != nil {
			log.Print(err)
			err = h.finishDeploymentAction(deploymentId, deploymentActionId, RestoringFailed, err.Error())
			if err != nil {
				log.Print(err)
			}
			return
		}
		err = h.finishDeploymentAction(deploymentId, deploymentActionId, Creating, "")
		if err != nil {
			log.Print(err)
			return
		}
		log.Printf("Deployment %s successfully restored", deploymentName)
	}()
}

func saveRestoreAction(db *sql.DB, deploymentName string) (int, int, error) {
	tx, err := db.Begin()
	if err != nil {
		return 0, 0, fmt.Errorf("Failed to begin transaction: %v", err)
	}
	defer tx.Rollback()

	_, err = tx.Exec(`
        UPDATE deployment_status
        SET status = $1, is_pending = TRUE, updated_at = $2
        WHERE deployment_id = (
            SELECT id FROM deployment WHERE name = $3
        )
    `, Restoring, time.Now(), deploymentName)
	if err != nil {
		return 0, 0, fmt.Errorf("Failed to update status for deployment: %v", err)
	}

	var deploymentId int
	var deploymentActionId int
	err = tx.QueryRow(`
        INSERT INTO deployment_action (deployment_id, action, created_at)
        VALUES (
            (SELECT id FROM deployment WHERE name = $1),
            $2,
            $3
        )
        RETURNING id, deployment_id
    `, deploymentName, Restore, time.Now()).Scan(&deploymentActionId, &deploymentId)
	if err != nil {
		return 0, 0, fmt.Errorf("Failed to insert new action for deployment %s: %v", deploymentName, err)
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
    `, deploymentActionId, deploymentName)
	if err != nil {
		return 0, 0, fmt.Errorf("Failed to duplicate helm override for deployment %s: %v", deploymentName, err)
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
    `, deploymentActionId, deploymentName)
	if err != nil {
		return 0, 0, fmt.Errorf("Failed to duplicate helm values override for deployment %s: %v", deploymentName, err)
	}

	err = tx.Commit()
	if err != nil {
		return 0, 0, fmt.Errorf("Failed to commit transaction for deployment %s: %v", deploymentName, err)
	}
	return deploymentId, deploymentActionId, nil
}
