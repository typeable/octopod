package api

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
	helmclient "github.com/mittwald/go-helm-client"
	"gopkg.in/yaml.v2"
)

func (h *Handler) UpdateDeploymentHandler(c *gin.Context) {
	var deployment DeploymentData
	if err := c.ShouldBindJSON(&deployment); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Invalid request payload"})
		return
	}

	if err := h.addOrUpdateHelmRepo(c); err != nil {
		return
	}

	chartName, chartVersion, values, err := h.prepareHelmSpec(c, &deployment)
	if err != nil {
		return
	}

	if err := h.upgradeChart(c, &deployment, chartName, chartVersion, values); err != nil {
		if err := addUpdateAction(h.Postgres, deployment, []Link{}, err.Error()); err != nil {
			return
		}

		c.JSON(http.StatusInternalServerError, gin.H{"message": "something went wrong"})
	} else {
		links, err := h.getIngressLinks(c, deployment.Name)
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to get ingress links"})
			return
		}

		if err := addUpdateAction(h.Postgres, deployment, links, ""); err != nil {
			return
		}

		c.JSON(http.StatusOK, gin.H{"message": "Deployment updated successfully"})
	}

}

func setStatusUpdating(db *sql.DB, deploymentName string) error {
	_, err := db.Exec(`
		UPDATE deployment_status
		SET status = $1, is_pending = TRUE, updated_at = $2
		WHERE deployment_id = (
			SELECT id FROM deployment WHERE name = $3
		)
	`, Updating, time.Now(), deploymentName)
	if err != nil {
		return fmt.Errorf("failed to update status: %v", err)
	}
	return nil
}

func (h *Handler) upgradeChart(c *gin.Context, deployment *DeploymentData, chartName, chartVersion string, values map[interface{}]interface{}) error {
	stringValues, err := yaml.Marshal(values)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Internal error"})
		return err
	}

	spec := &helmclient.ChartSpec{
		ReleaseName:   deployment.Name,
		ChartName:     chartName,
		Namespace:     h.Config.ReleaseNamespace,
		Wait:          false,
		Version:       chartVersion,
		ValuesYaml:    string(stringValues),
		CleanupOnFail: true,
	}

	_, err = h.HelmClient.UpgradeChart(c, spec, nil)
	if err != nil {
		log.Println(err)
		c.JSON(http.StatusBadRequest, gin.H{"error": err})
	}
	return err
}

func addUpdateAction(db *sql.DB, deployment DeploymentData, links []Link, helmError string) error {
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
    `, Updating, time.Now(), deployment.Name)
	if err != nil {
		return fmt.Errorf("failed to update status: %v", err)
	}

	var deploymentActionId int
	err = tx.QueryRow(`
        INSERT INTO deployment_action (deployment_id, action, created_at, error)
        VALUES (
            (SELECT id FROM deployment WHERE name = $1),
            $2,
            $3,
			$4
        )
        RETURNING id
    `, deployment.Name, Update, time.Now(), helmError).Scan(&deploymentActionId)
	if err != nil {
		return fmt.Errorf("failed to insert new action: %v", err)
	}
	log.Println(deploymentActionId)

	if deployment.ChartVersionOverride != "" {
		_, err := tx.Exec(`
			INSERT INTO deployment_helm_override (deployment_action_id, version)
			VALUES ($1, $2)
		`, deploymentActionId, deployment.ChartVersionOverride)
		if err != nil {
			log.Println(err)
			return err
		}
	}

	for _, override := range deployment.ValuesOverride {
		_, err := tx.Exec(`
			INSERT INTO deployment_helm_values_override (deployment_action_id, action, key, value)
			VALUES ($1, $2, $3, $4)
		`, deploymentActionId, override.OverrideAction, override.Name, override.Value)
		if err != nil {
			log.Println(err)
			return err
		}
	}
	for _, link := range links {
		_, err := tx.Exec(`
			INSERT INTO deployment_link (deployment_id, name, url)
			VALUES ((SELECT id FROM deployment WHERE name = $1), $2, $3)
		`, deployment.Name, link.Name, link.URL)
		if err != nil {
			log.Println(err)
			return err
		}
	}

	if err := tx.Commit(); err != nil {
		return err
	}

	return nil
}
