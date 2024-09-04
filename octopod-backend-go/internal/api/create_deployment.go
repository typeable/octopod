package api

import (
	"database/sql"
	"log"
	"net/http"
	"time"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
	helmclient "github.com/mittwald/go-helm-client"
	"gopkg.in/yaml.v2"
)

func (h *Handler) CreateDeploymentHandler(c *gin.Context) {
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

	if err := h.installChart(c, &deployment, chartName, chartVersion, values); err != nil {
		return
	}

	links, err := h.getIngressLinks(c, deployment.Name)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to get ingress links"})
		return
	}

	if err := h.saveDeploymentData(c, &deployment, links); err != nil {
		return
	}

	c.JSON(http.StatusOK, gin.H{"message": "Deployment created successfully"})
}

func (h *Handler) installChart(c *gin.Context, deployment *DeploymentData, chartName, chartVersion string, values map[interface{}]interface{}) error {
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

	_, err = h.HelmClient.InstallChart(c, spec, nil)
	if err != nil {
		log.Println(err)
		c.JSON(http.StatusBadRequest, gin.H{"error": err})
	}
	return err
}

func (h *Handler) saveDeploymentData(c *gin.Context, deployment *DeploymentData, links []Link) error {
	tx, err := h.Postgres.Begin()
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to start transaction"})
		return err
	}
	defer tx.Rollback()

	err = h.insertDeploymentRecords(tx, deployment, links)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to insert deployment data"})
		return err
	}

	if err := tx.Commit(); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to commit transaction"})
		return err
	}

	return nil
}

func (h *Handler) insertDeploymentRecords(tx *sql.Tx, deployment *DeploymentData, links []Link) error {
	var deploymentId int
	var deploymentActionId int

	err := tx.QueryRow(`
		INSERT INTO deployment (name, created_at)
		VALUES ($1, $2) RETURNING id
	`, deployment.Name, time.Now()).Scan(&deploymentId)
	if err != nil {
		log.Println(err)
		return err
	}

	err = tx.QueryRow(`
		INSERT INTO deployment_action (deployment_id, action, created_at)
		VALUES ($1, $2, $3) RETURNING id
	`, deploymentId, Create, time.Now()).Scan(&deploymentActionId)
	if err != nil {
		log.Println(err)
		return err
	}

	_, err = tx.Exec(`
		INSERT INTO deployment_status (deployment_id, status, is_pending, checked_at, updated_at)
		VALUES ($1, $2, $3, $4, $5)
	`, deploymentId, Creating, false, time.Now(), time.Now())
	if err != nil {
		log.Println(err)
		return err
	}

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
			VALUES ($1, $2, $3)
		`, deploymentId, link.Name, link.URL)
		if err != nil {
			log.Println(err)
			return err
		}
	}

	return nil
}
