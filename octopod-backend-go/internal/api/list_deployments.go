package api

import (
	"database/sql"
	"log"
	"net/http"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
)

func (h *Handler) ListDeploymentsHandler(c *gin.Context) {
	rows, err := executeQuery(h.Postgres)
	if err != nil {
		log.Print(err)
	}
	defer rows.Close()

	deployments, err := processDeployments(rows)
	if err != nil {
		log.Print(err)
	}

	c.JSON(http.StatusOK, deployments)
}

func executeQuery(postgres *sql.DB) (*sql.Rows, error) {
	return postgres.Query(`
		SELECT
			d.name AS deployment_name,
			d.created_at AS action_created_at,
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
			deployment_status ds ON ds.deployment_id = d.id
		JOIN
			deployment_action da ON da.deployment_id = d.id
		LEFT JOIN
			deployment_link dl ON dl.deployment_id = d.id
		LEFT JOIN
			deployment_helm_override dho ON dho.deployment_action_id = da.id
		LEFT JOIN
			deployment_helm_values_override dhvo ON dhvo.deployment_action_id = da.id
		WHERE
			da.id = (
				SELECT
					MAX(da_inner.id)
				FROM
					deployment_action da_inner
				WHERE
					da_inner.deployment_id = d.id
			);`)
}
