package api

import (
	"github.com/gin-gonic/gin"
)

// RegisterRoutes регистрирует все маршруты с использованием переданного Handler
func RegisterRoutes(r *gin.Engine, handler *Handler) {
	api := r.Group("/api/v1")
	{
		api.GET("/ping", handler.PingHandler)
		api.GET("/project_name", handler.GetProjectNameHandler)

		deployments := api.Group("/deployments")
		{
			deployments.GET("", handler.ListDeploymentsHandler)
			deployments.POST("", handler.CreateDeploymentHandler)
			// 	deployments.DELETE("/:name", handler.ArchiveDeploymentHandler)
			// 	deployments.PUT("/:name", handler.UpdateDeploymentHandler)
			// 	deployments.GET("/:name/info", handler.GetDeploymentInfoHandler)
			// 	deployments.GET("/:name/full_info", handler.GetDeploymentFullInfoHandler)
			// 	deployments.GET("/:name/status", handler.GetDeploymentStatusHandler)
			// 	deployments.PATCH("/:name/restore", handler.RestoreDeploymentHandler)
		}

		// api.GET("/deployment_override_keys", handler.GetDefaultDeploymentOverrideKeysHandler)
		// api.GET("/deployment_overrides", handler.GetDefaultDeploymentOverridesHandler)
		// api.POST("/application_override_keys", handler.GetDefaultApplicationOverrideKeysHandler)
		// api.POST("/application_overrides", handler.GetDefaultApplicationOverridesHandler)
	}
}
