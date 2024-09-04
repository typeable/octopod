package api

import (
	"log"
	"net/http"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
)

func (h *Handler) CleanupDeploymentHandler(c *gin.Context) {
	deploymentName := c.Param("name")

	if err := h.uninstallChart(c, deploymentName); err != nil {
		return
	}
	h.deleteDeploymentData(c, deploymentName)

	c.JSON(http.StatusOK, gin.H{"message": "Deployment deleted successfully"})
}

func (h *Handler) uninstallChart(c *gin.Context, deploymentName string) error {
	err := h.HelmClient.UninstallReleaseByName(deploymentName)
	if err != nil {
		log.Println(err)
		c.JSON(http.StatusBadRequest, gin.H{"error": err})
	}
	return err
}

func (h *Handler) deleteDeploymentData(c *gin.Context, deploymentName string) error {
	tx, err := h.Postgres.Begin()
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to start transaction"})
		return err
	}
	defer tx.Rollback()

	_, err = tx.Exec(`
		DELETE FROM deployment
		WHERE name = $1
	`, deploymentName)
	if err != nil {
		log.Println(err)
		return err
	}
	if err := tx.Commit(); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to commit transaction"})
		return err
	}

	return nil
}
