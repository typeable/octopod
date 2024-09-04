package api

import (
	"database/sql"
	"net/http"
	"octopod-backend/internal/config"

	"github.com/gin-gonic/gin"
	"k8s.io/client-go/kubernetes"

	helmclient "github.com/mittwald/go-helm-client"
)

type Handler struct {
	Config            *config.Config
	HelmClient        helmclient.Client
	K8sClient         *kubernetes.Clientset
	Postgres          *sql.DB
	LockedDeployments *[]string
}

func NewHandler(cfg *config.Config, hc helmclient.Client, k8sClient *kubernetes.Clientset, p *sql.DB) *Handler {
	return &Handler{
		HelmClient:        hc,
		K8sClient:         k8sClient,
		Config:            cfg,
		Postgres:          p,
		LockedDeployments: new([]string),
	}
}

func (h *Handler) PingHandler(c *gin.Context) {
	c.String(http.StatusOK, "pong")
}

func (h *Handler) GetProjectNameHandler(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{"project_name": h.Config.ProjectName})
}

func RegisterRoutes(r *gin.Engine, handler *Handler) {
	api := r.Group("/api/v1")
	{
		api.GET("/ping", handler.PingHandler)
		api.GET("/project_name", handler.GetProjectNameHandler)

		deployments := api.Group("/deployments")
		{
			deployments.GET("", handler.ListDeploymentsHandler)
			deployments.POST("", handler.CreateDeploymentHandler)
			deployments.DELETE("/:name", handler.ArchiveDeploymentHandler)
			deployments.PUT("", handler.UpdateDeploymentHandler)
			deployments.GET("/:name", handler.GetDeploymentHandler)
			deployments.GET("/:name/actions", handler.GetDeploymentActionsHandler)
			deployments.GET("/:name/status", handler.GetDeploymentStatusHandler)
			deployments.PATCH("/:name", handler.RestoreDeploymentHandler)
			deployments.DELETE("/:name/cleanup", handler.CleanupDeploymentHandler)
		}

		// api.GET("/deployment_override_keys", handler.GetDefaultDeploymentOverrideKeysHandler)
		// api.GET("/deployment_overrides", handler.GetDefaultDeploymentOverridesHandler)
		// api.POST("/application_override_keys", handler.GetDefaultApplicationOverrideKeysHandler)
		// api.POST("/application_overrides", handler.GetDefaultApplicationOverridesHandler)
	}
}
