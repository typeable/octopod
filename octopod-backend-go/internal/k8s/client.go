package k8s

import (
	"log"
	"os"

	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"
	"k8s.io/client-go/tools/clientcmd"
)

func NewK8sClient() *kubernetes.Clientset {
	k8sConfig, err := rest.InClusterConfig()
	if err == nil {
		client, err := kubernetes.NewForConfig(k8sConfig)
		if err != nil {
			panic(err.Error())
		}

		return client
	} else if err == rest.ErrNotInCluster {
		config := os.Getenv("KUBECONFIG")
		k8sConfig, err := clientcmd.BuildConfigFromFlags("", config)
		if err != nil {
			panic(err.Error())
		}
		clientset, err := kubernetes.NewForConfig(k8sConfig)
		if err != nil {
			panic(err.Error())
		}
		return clientset
	} else {
		log.Fatalf("Could not create Kubernetes config: %s", err)
	}
	return nil
}
