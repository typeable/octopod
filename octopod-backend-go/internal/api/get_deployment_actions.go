package api

import (
	"database/sql"
	"log"
	"maps"
	"net/http"
	"slices"
	"sort"
	"time"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
)

func (h *Handler) GetDeploymentActionsHandler(c *gin.Context) {
	deploymentName := c.Param("name")
	rows, err := queryDeploymentActions(h.Postgres, deploymentName)
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	deploymentActionsMap, err := processDeploymentActions(rows)
	if err != nil {
		log.Fatal(err)
	}

	deploymentActions := slices.Collect(maps.Values(deploymentActionsMap))

	sort.SliceStable(deploymentActions, func(i, j int) bool {
		return deploymentActions[i].CreatedAt.After(deploymentActions[j].CreatedAt)
	})

	c.JSON(http.StatusOK, deploymentActions)
}

func queryDeploymentActions(postgres *sql.DB, deploymentName string) (*sql.Rows, error) {
	return postgres.Query(`
		SELECT
			da.action AS action,
			da.created_at AS action_created_at,
			da.error AS error,
			dho.version AS helm_version,
			dhvo.action AS helm_action,
			dhvo.key AS helm_key,
			dhvo.value AS helm_value
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
			d.name = $1`, deploymentName)
}

func processDeploymentActions(rows *sql.Rows) (map[time.Time]*DeploymentAction, error) {
	deploymentActions := make(map[time.Time]*DeploymentAction)

	for rows.Next() {
		var (
			action          string
			createdAt       time.Time
			helmError       sql.NullString
			helmVersion     sql.NullString
			helmValueAction sql.NullString
			helmKey         sql.NullString
			helmValue       sql.NullString
		)
		if err := rows.Scan(
			&action,
			&createdAt,
			&helmError,
			&helmVersion,
			&helmValueAction,
			&helmKey,
			&helmValue,
		); err != nil {
			return nil, err
		}

		if err := updateDeploymentAction(deploymentActions, Action(action), createdAt, helmError, helmVersion, helmValueAction, helmKey, helmValue); err != nil {
			return nil, err
		}
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return deploymentActions, nil
}

func updateDeploymentAction(deploymentActions map[time.Time]*DeploymentAction, action Action, createdAt time.Time, helmError sql.NullString, helmVersion sql.NullString, helmValueAction sql.NullString, helmKey sql.NullString, helmValue sql.NullString) error {
	deploymentAction, exists := deploymentActions[createdAt]
	if !exists {
		deploymentAction = &DeploymentAction{
			Action:    action,
			CreatedAt: createdAt,
		}
		deploymentActions[createdAt] = deploymentAction
	}

	if helmValueAction.Valid && helmKey.Valid && helmValue.Valid {
		addHelmOverrideAction(deploymentAction, helmValueAction.String, helmKey.String, helmValue.String)
	}

	if helmVersion.Valid {
		deploymentAction.ChartVersionOverride = helmVersion.String
	}

	if helmError.Valid {
		deploymentAction.Error = helmError.String
	}

	return nil
}

func addHelmOverrideAction(deploymentAction *DeploymentAction, action, key, value string) {
	exists := false
	for _, o := range deploymentAction.ValuesOverride {
		if o.Name == key {
			exists = true
			break
		}
	}
	if !exists {
		override := Override{
			Name:           key,
			OverrideAction: OverrideAction(action),
			Value:          value,
		}
		deploymentAction.ValuesOverride = append(deploymentAction.ValuesOverride, override)
	}
}
