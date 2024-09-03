package helm

import (
	"log"

	helmclient "github.com/mittwald/go-helm-client"
	"k8s.io/client-go/rest"
)

func NewHelmClient(namespace string) helmclient.Client {
	k8sConfig, err := rest.InClusterConfig()
	if err == nil {
		opt := &helmclient.RestConfClientOptions{
			Options: &helmclient.Options{
				Namespace:        namespace,
				RepositoryCache:  "/tmp/.helmcache",
				RepositoryConfig: "/tmp/.helmrepo",
			},
			RestConfig: k8sConfig,
		}

		client, err := helmclient.NewClientFromRestConf(opt)
		if err != nil {
			log.Fatalf("Could not create Helm client: %s", err)
		}

		return client
	} else if err == rest.ErrNotInCluster {
		opt := &helmclient.Options{
			Namespace:        namespace,
			RepositoryCache:  "/tmp/.helmcache",
			RepositoryConfig: "/tmp/.helmrepo",
		}
		client, err := helmclient.New(opt)
		if err != nil {
			log.Fatalf("Could not create Helm client: %s", err)
		}
		return client
	} else {
		log.Fatalf("Could not create Kubernetes config: %s", err)
	}
	return nil
}
