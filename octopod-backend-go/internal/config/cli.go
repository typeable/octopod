package config

import (
	"flag"
	"log"
)

type Config struct {
	ProjectName             string
	ReleaseNamespace        string
	HelmRepo                string
	HelmRepoUser            string
	HelmRepoPassword        string
	Port                    int
	UIPort                  int
	WSPort                  int
	DB                      string
	DBMaxOpenConnections    int
	DBMaxIdleConnections    int
	DefaultHelmValues       string
	DefaultHelmChart        string
	DefaultHelmChartVersion string
}

func LoadConfig() *Config {
	config := &Config{}

	flag.StringVar(&config.ProjectName, "project-name", "Deployment manager", "Octopod project name")
	flag.StringVar(&config.ReleaseNamespace, "release-namespace", "default", "Kubernetes namespace for release")
	flag.IntVar(&config.Port, "port", 8080, "Port for the HTTP server")
	flag.IntVar(&config.UIPort, "ui-port", 8081, "Port for the UI")
	flag.IntVar(&config.WSPort, "ws-port", 8082, "Port for WebSocket server")
	flag.StringVar(&config.DB, "db", "", "Database connection string")
	flag.IntVar(&config.UIPort, "db-max-open-connections", 10, "Max open connections")
	flag.IntVar(&config.UIPort, "db-max-idle-connections", 10, "Max idle connections")
	flag.StringVar(&config.HelmRepo, "helm-repo", "", "Helm repository")
	flag.StringVar(&config.HelmRepoUser, "helm-repo-user", "", "Helm repository user")
	flag.StringVar(&config.HelmRepoPassword, "helm-repo-password", "", "Helm repository password")
	flag.StringVar(&config.DefaultHelmChart, "default-helm-chart", "", "Default helm chart")
	flag.StringVar(&config.DefaultHelmChartVersion, "default-deployment-overrides", "", "Default helm chart version")
	flag.StringVar(&config.DefaultHelmValues, "default-values", "", "Default values")

	flag.Parse()

	if config.DB == "" {
		log.Fatal("Error: --db is required")
	}
	if config.HelmRepo == "" {
		log.Fatal("Error: --helm-repo is required")
	}
	if config.HelmRepoUser == "" {
		log.Fatal("Error: --helm-repo-user is required")
	}
	if config.HelmRepoPassword == "" {
		log.Fatal("Error: --helm-repo-password is required")
	}
	if config.DefaultHelmChart == "" {
		log.Fatal("Error: --default-helm-chart is required")
	}
	if config.DefaultHelmChartVersion == "" {
		log.Fatal("Error: --default-deployment-overrides is required")
	}
	if config.DefaultHelmValues == "" {
		log.Fatal("Error: --default-values is required")
	}

	return config
}
