package api

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"octopod-backend/internal/config"
	"regexp"
	"slices"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
	helmclient "github.com/mittwald/go-helm-client"
	"helm.sh/helm/v3/pkg/repo"
)

// Handler содержит зависимости для обработчиков
type Handler struct {
	Config            *config.Config
	HelmClient        helmclient.Client
	Postgres          *sql.DB
	LockedDeployments *[]string
}

// NewHandler создает новый экземпляр Handler
func NewHandler(cfg *config.Config, hc helmclient.Client, p *sql.DB) *Handler {
	return &Handler{
		HelmClient:        hc,
		Config:            cfg,
		Postgres:          p,
		LockedDeployments: new([]string),
	}
}

// PingHandler отвечает "pong"
func (h *Handler) PingHandler(c *gin.Context) {
	c.String(http.StatusOK, "pong")
}

// GetProjectNameHandler возвращает название проекта
func (h *Handler) GetProjectNameHandler(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{"project_name": h.Config.ProjectName})
}

type Link struct {
	Name string `json:"name"`
	Link string `json:"link"`
}

type OverrideValue struct {
	Tag      string `json:"tag"`
	Contents string `json:"contents"`
}

type Override struct {
	Name  string        `json:"name"`
	Value OverrideValue `json:"value"`
}

type Status struct {
	Pending bool   `json:"pending"`
	Status  string `json:"status"`
}

type DeploymentData struct {
	Name                string     `json:"name"`
	AppOverrides        []Override `json:"app_overrides"`
	DeploymentOverrides []Override `json:"deployment_overrides"`
}

type Deployment struct {
	DeploymentData DeploymentData `json:"deployment_data"`
	Status         Status         `json:"status"`
	Links          []Link         `json:"metadata"`
}

func (h *Handler) ListDeploymentsHandler(c *gin.Context) {
	rows, err := h.Postgres.Query("SELECT name, app_overrides, deployment_overrides, status, links FROM deployments")
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	for rows.Next() {
		var (
			name                   string
			statusRaw              string
			appOverridesRaw        []byte
			deploymentOverridesRaw []byte
			linksRaw               []byte
		)
		if err := rows.Scan(&name, &appOverridesRaw, &deploymentOverridesRaw, &statusRaw, &linksRaw); err != nil {
			log.Fatal(err)
		}

		links, err := unmarshalLinks(linksRaw)
		if err != nil {
			log.Fatal(err)
		}

		appOverrides, err := unmarshalOverrides(appOverridesRaw)
		if err != nil {
			log.Fatal(err)
		}

		deploymentOverrides, err := unmarshalOverrides(deploymentOverridesRaw)
		if err != nil {
			log.Fatal(err)
		}

		status := Status{
			Pending: isLocked(name, h.LockedDeployments),
			Status:  statusRaw,
		}

		deploymentData := DeploymentData{
			Name:                name,
			AppOverrides:        appOverrides,
			DeploymentOverrides: deploymentOverrides,
		}

		deployment := Deployment{
			DeploymentData: deploymentData,
			Status:         status,
			Links:          links,
		}

		c.JSON(http.StatusOK, deployment)
	}
}

func unmarshalLinks(data []byte) ([]Link, error) {
	var links []Link
	if err := json.Unmarshal(data, &links); err != nil {
		return nil, err
	}
	return links, nil
}

func unmarshalOverrides(data []byte) ([]Override, error) {
	var pairs [][]interface{}
	if err := json.Unmarshal(data, &pairs); err != nil {
		return nil, err
	}

	var overrides []Override
	for _, pair := range pairs {
		name, ok := pair[0].(string)
		if !ok {
			return nil, fmt.Errorf("invalid override name")
		}

		value, err := unmarshalOverrideValue(pair[1])
		if err != nil {
			return nil, err
		}

		overrides = append(overrides, Override{Name: name, Value: value})
	}
	return overrides, nil
}

func unmarshalOverrideValue(data interface{}) (OverrideValue, error) {
	valueData, err := json.Marshal(data)
	if err != nil {
		return OverrideValue{}, err
	}

	var value OverrideValue
	if err := json.Unmarshal(valueData, &value); err != nil {
		return OverrideValue{}, err
	}

	return value, nil
}

func isLocked(name string, lockedDeployments *[]string) bool {
	return slices.Contains(*lockedDeployments, name)
}

func ReplaceNonAlphanumeric(input string) string {
	// Создаем регулярное выражение для поиска всех символов, которые не являются буквами или цифрами
	re := regexp.MustCompile(`[^\p{L}\p{N}]+`)

	// Заменяем все найденные символы на нижнее подчеркивание
	return re.ReplaceAllString(input, "_")
}

// type Deployment struct {
// 	Name                string     `json:"name"`
// 	AppOverrides        []Override `json:"app_overrides"`
// 	DeploymentOverrides []Override `json:"deployment_overrides"`
// }

func (h *Handler) CreateDeploymentHandler(c *gin.Context) {
	var deployment DeploymentData
	if err := c.ShouldBindJSON(&deployment); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}

	err := h.HelmClient.AddOrUpdateChartRepo(repo.Entry{
		Name:               ReplaceNonAlphanumeric(h.Config.HelmRepo),
		URL:                h.Config.HelmRepo,
		PassCredentialsAll: true,
	})
	if err != nil {
		log.Fatal(err)
	}

	c.JSON(http.StatusOK, gin.H{"status": "deployment created"})
}
