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

func (h *Handler) CreateDeploymentHandler(c *gin.Context) {

	var deployment DeploymentData
	if err := c.ShouldBindJSON(&deployment); err != nil {
		log.Print(err)
		c.JSON(http.StatusBadRequest, ginError("Invalid request payload"))
		return
	}
	log.SetPrefix(deployment.Name)
	log.Printf("Starting deployment creation")

	if err := h.addOrUpdateHelmRepo(c); err != nil {
		log.Print(err)
		c.JSON(http.StatusInternalServerError, ginError("Something went wrong"))
		return
	}

	chartName, chartVersion, values, err := h.prepareHelmSpec(c, &deployment)
	if err != nil {
		log.Print(err)
		c.JSON(http.StatusInternalServerError, ginError("Something went wrong"))
		return
	}

	deploymentId, deploymentActionId, err := h.saveCreateAction(&deployment)
	if err != nil {
		log.Print(err)
		c.JSON(http.StatusInternalServerError, ginError("Something went wrong"))
		return
	}

	c.JSON(http.StatusOK, ginHelmProcess())
	log.Println("Deployment process started")

	go func() {
		if err := h.installChart(c, &deployment, chartName, chartVersion, values); err != nil {
			log.Print(err)
			h.finishDeploymentAction(deploymentId, deploymentActionId, CreatingFailed, err.Error())
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

		links, err := h.getIngressLinks(c, deployment.Name)
		if err != nil {
			log.Print(err)
			return
		}

		if err = h.saveDeploymentLinks(deploymentId, links); err != nil {
			log.Print(err)
			c.JSON(http.StatusOK, ginError("Something went wrong"))
			return
		}

		log.Printf("Deployment successfully created")
	}()
}

func (h *Handler) installChart(c *gin.Context, deployment *DeploymentData, chartName, chartVersion string, values map[interface{}]interface{}) error {
	stringValues, err := yaml.Marshal(values)
	if err != nil {
		return fmt.Errorf("Error marshalling helm values: %s", err)
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
		return fmt.Errorf("Error installing chart: %s", err)
	}

	return nil
}

func (h *Handler) saveCreateAction(deployment *DeploymentData) (int, int, error) {
	tx, err := h.Postgres.Begin()
	if err != nil {
		return 0, 0, fmt.Errorf("Error starting transaction: %s", err)
	}
	defer tx.Rollback()

	deploymentId, deploymentActionId, err := h.saveCreateActionQueries(tx, deployment)
	if err != nil {
		return 0, 0, fmt.Errorf("Error inserting deployment records: %s", err)
	}

	if err := tx.Commit(); err != nil {
		return 0, 0, fmt.Errorf("Error committing transaction: %s", err)
	}

	return deploymentId, deploymentActionId, nil
}

func (h *Handler) saveCreateActionQueries(tx *sql.Tx, deployment *DeploymentData) (int, int, error) {
	var deploymentId int
	var deploymentActionId int

	err := tx.QueryRow(`
		INSERT INTO deployment (name, created_at)
		VALUES ($1, $2) RETURNING id
	`, deployment.Name, time.Now()).Scan(&deploymentId)
	if err != nil {
		return 0, 0, fmt.Errorf("Error inserting into deployment table: %s", err)
	}

	_, err = tx.Exec(`
		INSERT INTO deployment_status (deployment_id, status, is_pending, checked_at, updated_at)
		VALUES ($1, $2, $3, $4, $5)
	`, deploymentId, Creating, true, time.Now(), time.Now())
	if err != nil {
		return 0, 0, fmt.Errorf("Error inserting into deployment_status table: %s", err)
	}

	err = tx.QueryRow(`
		INSERT INTO deployment_action (deployment_id, action, created_at)
		VALUES ($1, $2, $3) RETURNING id
	`, deploymentId, Create, time.Now()).Scan(&deploymentActionId)
	if err != nil {
		return 0, 0, fmt.Errorf("Error inserting into deployment_action table: %s", err)
	}

	if deployment.ChartVersionOverride != "" {
		_, err := tx.Exec(`
			INSERT INTO deployment_helm_override (deployment_action_id, version)
			VALUES ($1, $2)
		`, deploymentActionId, deployment.ChartVersionOverride)
		if err != nil {
			return 0, 0, fmt.Errorf("Error inserting into deployment_helm_override table: %s", err)
		}
	}

	for _, override := range deployment.ValuesOverride {
		_, err := tx.Exec(`
			INSERT INTO deployment_helm_values_override (deployment_action_id, action, key, value)
			VALUES ($1, $2, $3, $4)
		`, deploymentActionId, override.OverrideAction, override.Name, override.Value)
		if err != nil {
			return 0, 0, fmt.Errorf("Error inserting into deployment_helm_values_override table: %s", err)
		}
	}

	return deploymentId, deploymentActionId, nil
}
