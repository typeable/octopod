package main

import (
	"log"
	"strconv"

	"octopod-backend/internal/api"
	"octopod-backend/internal/config"
	"octopod-backend/internal/helm"
	"octopod-backend/internal/k8s"
	"octopod-backend/internal/postgres"

	"github.com/gin-gonic/gin"
	_ "github.com/lib/pq"
)

func main() {
	cfg := config.LoadConfig()

	helmClient := helm.NewHelmClient(cfg.ReleaseNamespace)
	k8sClient := k8s.NewK8sClient()

	db := postgres.NewDBConnection(cfg.DB, cfg.DBMaxOpenConnections, cfg.DBMaxIdleConnections)
	defer func() {
		if err := db.Close(); err != nil {
			log.Fatalf("Failed to close database connection: %v", err)
		}
	}()

	handler := api.NewHandler(cfg, helmClient, k8sClient, db)

	r := gin.Default()

	api.RegisterRoutes(r, handler)

	if err := r.Run(":" + strconv.Itoa(cfg.Port)); err != nil {
		log.Fatalf("Could not start server: %s", err)
	}
}
