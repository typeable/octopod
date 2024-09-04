package api

import (
	"database/sql"
	"log"
	"net/http"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
)

func (h *Handler) GetDeploymentHandler(c *gin.Context) {
	deploymentName := c.Param("name")
	rows, err := queryDeployment(h.Postgres, deploymentName)
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	deployments, err := processDeployments(rows)
	if err != nil {
		log.Fatal(err)
	}

	if deployment, ok := deployments[deploymentName]; ok {
		c.JSON(http.StatusOK, deployment)
	} else {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "something went wrong"})
	}
}

func queryDeployment(postgres *sql.DB, deploymentName string) (*sql.Rows, error) {
	return postgres.Query(`
		SELECT
			d.name AS deployment_name,
			da.created_at AS action_created_at,
			ds.status AS last_status,
			ds.is_pending,
			dho.version AS helm_version,
			dhvo.action AS helm_action,
			dhvo.key AS helm_key,
			dhvo.value AS helm_value,
			dl.name as link_name,
			dl.url as link_url
		FROM
			deployment d
		JOIN
			deployment_action da ON da.deployment_id = d.id
		JOIN
			deployment_status ds ON ds.deployment_id = d.id
		LEFT JOIN
			deployment_link dl ON dl.deployment_id = d.id
		LEFT JOIN
			deployment_helm_override dho ON dho.deployment_action_id = da.id
		LEFT JOIN
			deployment_helm_values_override dhvo ON dhvo.deployment_action_id = da.id
		WHERE
			d.name = $1
			AND da.id = (
				SELECT
					MAX(da_inner.id)
				FROM
					deployment_action da_inner
				WHERE
					da_inner.deployment_id = d.id
			);`, deploymentName)
}
