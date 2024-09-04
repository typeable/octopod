package api

import (
	"fmt"
	"log"
	"net/http"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
)

func (h *Handler) CleanupDeploymentHandler(c *gin.Context) {
	log.Println("Starting deployment cleanup")

	deploymentName := c.Param("name")
	if deploymentName == "" {
		log.Printf("Error: Deployment name is required")
		c.JSON(http.StatusBadRequest, ginError("Invalid request payload"))
		return
	}
	log.SetPrefix(deploymentName)

	err := h.uninstallChart(deploymentName)
	if err != nil {
		log.Print(err)
		c.JSON(http.StatusInternalServerError, ginError("Cannot uninstall release"))
		return
	}

	err = h.deleteDeploymentData(deploymentName)
	if err != nil {
		log.Print(err)
		c.JSON(http.StatusInternalServerError, ginError("Cannot delete deployment data"))
		return
	}

	c.JSON(http.StatusOK, ginMessage("Deployment deleted successfully"))
	log.Println("Finished deployment cleanup")
}

func (h *Handler) uninstallChart(deploymentName string) error {
	err := h.HelmClient.UninstallReleaseByName(deploymentName)
	if err != nil {
		return fmt.Errorf("Error during Helm release uninstallation for deployment: %v", err)
	}
	return nil
}

func (h *Handler) deleteDeploymentData(deploymentName string) error {
	tx, err := h.Postgres.Begin()
	if err != nil {
		return fmt.Errorf("Error starting transaction for deleting deployment: %v", err)
	}
	defer tx.Rollback()

	_, err = tx.Exec(`
		DELETE FROM deployment
		WHERE name = $1
	`, deploymentName)
	if err != nil {
		return fmt.Errorf("Error deleting deployment record for deployment: %v", err)
	}

	if err := tx.Commit(); err != nil {
		return fmt.Errorf("Error committing transaction for deployment: %v", err)
	}

	return nil
}
