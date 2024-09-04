package config

import (
	"flag"
	"log"
	"os"

	"gopkg.in/yaml.v2"
)

type Config struct {
	ProjectName             string
	ReleaseNamespace        string
	Port                    int
	UIPort                  int
	WSPort                  int
	DB                      string
	DBMaxOpenConnections    int
	DBMaxIdleConnections    int
	HelmRepo                string
	HelmRepoUser            string
	HelmRepoPassword        string
	HelmChart               string
	DefaultHelmValues       map[interface{}]interface{}
	DefaultHelmChartVersion string
	BaseDomain              string
	BaseDomainKey           string
}

func LoadConfig() *Config {
	config := &Config{}

	var defaultHelmValuesFile string

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
	flag.StringVar(&config.HelmChart, "default-helm-chart", "", "Default helm chart")
	flag.StringVar(&config.DefaultHelmChartVersion, "default-helm-chart-version", "", "Default helm chart version")
	flag.StringVar(&defaultHelmValuesFile, "default-helm-values", "", "Default values")
	flag.StringVar(&config.BaseDomain, "base-domain", "", "Base domain")
	flag.StringVar(&config.BaseDomainKey, "base-domain-key", "", "Base domain path in values")

	flag.Parse()

	if config.DB == "" {
		log.Fatal("Error: --db is required")
	}
	if config.HelmRepo == "" {
		log.Fatal("Error: --helm-repo is required")
	}
	if config.HelmChart == "" {
		log.Fatal("Error: --default-helm-chart is required")
	}
	if config.DefaultHelmChartVersion == "" {
		log.Fatal("Error: --default-deployment-overrides is required")
	}
	if defaultHelmValuesFile != "" {
		yamlFile, err := os.ReadFile(defaultHelmValuesFile)
		if err != nil {
			log.Fatal("Error: bad default helm values filepath")
		}
		err = yaml.Unmarshal([]byte(yamlFile), &config.DefaultHelmValues)
		if err != nil {
			log.Fatal("Error: bad default helm values")
		}
	}

	return config
}
