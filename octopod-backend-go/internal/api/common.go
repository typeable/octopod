package api

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"regexp"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"helm.sh/helm/v3/pkg/repo"
	autoscalingv1 "k8s.io/api/autoscaling/v1"
	"k8s.io/apimachinery/pkg/api/errors"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	v1 "k8s.io/apimachinery/pkg/apis/meta/v1"
)

func applyOverride(data map[interface{}]interface{}, override Override) error {
	keys := strings.Split(override.Name, ".")

	lastMap := data
	for i, k := range keys {
		if i == len(keys)-1 {
			// Последний ключ в пути
			switch override.OverrideAction {
			case ValueAdd:
				lastMap[k] = override.Value
			case ValueDelete:
				delete(lastMap, k)
			}
		} else {
			if _, ok := lastMap[k]; !ok {
				if override.OverrideAction == ValueDelete {
					return nil
				}
				lastMap[k] = make(map[interface{}]interface{})
			}
			if m, ok := lastMap[k].(map[interface{}]interface{}); ok {
				lastMap = m
			} else {
				log.Fatalf("key %s is not a map", k)
			}
		}
	}
	return nil
}

func setStatus(p *sql.DB, deploymentName string, status Status) error {
	_, err := p.Exec(`
		UPDATE deployment_status  SET status = $1
		WHERE deployment_id = (SELECT id FROM deployment WHERE name = $2);
	`, status, deploymentName)
	if err != nil {
		log.Println(err)
		return err
	}

	return nil
}

func ReplaceNonAlphanumeric(input string) string {
	// Создаем регулярное выражение для поиска всех символов, которые не являются буквами или цифрами
	re := regexp.MustCompile(`[^\p{L}\p{N}]+`)

	// Заменяем все найденные символы на нижнее подчеркивание
	return re.ReplaceAllString(input, "_")
}

func (h *Handler) addOrUpdateHelmRepo(c *gin.Context) error {
	err := h.HelmClient.AddOrUpdateChartRepo(repo.Entry{
		Name:               ReplaceNonAlphanumeric(h.Config.HelmRepo),
		URL:                h.Config.HelmRepo,
		Username:           h.Config.HelmRepoUser,
		Password:           h.Config.HelmRepoPassword,
		PassCredentialsAll: true,
	})
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Internal error"})
	}
	return err
}

func (h *Handler) prepareHelmSpec(c *gin.Context, deployment *DeploymentData) (string, string, map[interface{}]interface{}, error) {
	chartName := ReplaceNonAlphanumeric(h.Config.HelmRepo) + "/" + h.Config.HelmChart
	chartVersion := h.Config.DefaultHelmChartVersion
	if deployment.ChartVersionOverride != "" {
		chartVersion = deployment.ChartVersionOverride
	}

	values := h.Config.DefaultHelmValues
	for _, override := range deployment.ValuesOverride {
		if err := applyOverride(values, override); err != nil {
			c.JSON(http.StatusBadRequest, gin.H{"error": "bad overrides"})
			return "", "", nil, err
		}
	}

	baseDomainOverride := Override{
		Name:           h.Config.BaseDomainKey,
		OverrideAction: ValueAdd,
		Value:          deployment.Name + "." + h.Config.BaseDomain,
	}
	if err := applyOverride(values, baseDomainOverride); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": "Internal error"})
		return "", "", nil, err
	}

	return chartName, chartVersion, values, nil
}

func (h *Handler) getIngressLinks(c *gin.Context, releaseName string) ([]Link, error) {
	ingressList, err := h.K8sClient.NetworkingV1().Ingresses(h.Config.ReleaseNamespace).List(c, v1.ListOptions{})
	if err != nil {
		log.Fatalf("Error listing Ingress resources: %s", err.Error())
		return nil, err
	}

	var links []Link
	for _, ingress := range ingressList.Items {
		annotations := ingress.GetAnnotations()
		if name, ok := annotations["meta.helm.sh/release-name"]; ok && name == releaseName {
			for _, rule := range ingress.Spec.Rules {
				links = append(links, Link{
					Name: ingress.Name,
					URL:  rule.Host,
				})
			}
		}
	}

	return links, nil
}

func processDeployments(rows *sql.Rows) (map[string]*Deployment, error) {
	deployments := make(map[string]*Deployment)

	for rows.Next() {
		var (
			deploymentName      string
			deploymentCreatedAt time.Time
			status              string
			isPending           bool
			helmVersion         sql.NullString
			helmValueAction     sql.NullString
			helmKey             sql.NullString
			helmValue           sql.NullString
			linkName            sql.NullString
			linkUrl             sql.NullString
		)
		if err := rows.Scan(
			&deploymentName,
			&deploymentCreatedAt,
			&status,
			&isPending,
			&helmVersion,
			&helmValueAction,
			&helmKey,
			&helmValue,
			&linkName,
			&linkUrl,
		); err != nil {
			return nil, err
		}

		if err := updateDeployment(deployments, deploymentName, isPending, Status(status), helmVersion, helmValueAction, helmKey, helmValue, linkName, linkUrl); err != nil {
			return nil, err
		}
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return deployments, nil
}

func updateDeployment(deployments map[string]*Deployment, name string, isPending bool, status Status, helmVersion sql.NullString, helmValueAction sql.NullString, helmKey sql.NullString, helmValue sql.NullString, linkName sql.NullString, linkUrl sql.NullString) error {
	deployment, exists := deployments[name]
	if !exists {
		deployment = &Deployment{
			DeploymentData: DeploymentData{
				Name: name,
			},

			Pending: isPending,
			Status:  status,

			Links: []Link{},
		}
		deployments[name] = deployment
	}

	if helmValueAction.Valid && helmKey.Valid && helmValue.Valid {
		addHelmOverride(deployment, helmValueAction.String, helmKey.String, helmValue.String)
	}

	if linkName.Valid && linkUrl.Valid {
		addLink(deployment, linkName.String, linkUrl.String)
	}

	if helmVersion.Valid {
		deployment.DeploymentData.ChartVersionOverride = helmVersion.String
	}

	return nil
}

func addHelmOverride(deployment *Deployment, action, key, value string) {
	exists := false
	for _, o := range deployment.DeploymentData.ValuesOverride {
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
		deployment.DeploymentData.ValuesOverride = append(deployment.DeploymentData.ValuesOverride, override)
	}
}

func addLink(deployment *Deployment, name, url string) {
	exists := false
	for _, l := range deployment.Links {
		if l.URL == url {
			exists = true
			break
		}
	}
	if !exists {
		link := Link{
			Name: name,
			URL:  url,
		}
		deployment.Links = append(deployment.Links, link)
	}
}

func (h *Handler) scaleResources(c *gin.Context, releaseName string, replicas int32) error {

	deployments, err := h.K8sClient.AppsV1().Deployments(h.Config.ReleaseNamespace).List(c, metav1.ListOptions{})
	if err != nil && !errors.IsNotFound(err) {
		return fmt.Errorf("failed to list deployments: %v", err)
	}
	for _, deployment := range deployments.Items {
		log.Println(deployment.Annotations["meta.helm.sh/release-name"], releaseName)
		if deployment.Annotations["meta.helm.sh/release-name"] == releaseName {
			scale := &autoscalingv1.Scale{
				ObjectMeta: metav1.ObjectMeta{
					Name:      deployment.Name,
					Namespace: deployment.Namespace,
				},
				Spec: autoscalingv1.ScaleSpec{
					Replicas: replicas,
				},
			}
			_, err := h.K8sClient.AppsV1().Deployments(deployment.Namespace).UpdateScale(c, deployment.Name, scale, metav1.UpdateOptions{})
			if err != nil {
				return fmt.Errorf("failed to scale deployment %s/%s to zero: %v", deployment.Namespace, deployment.Name, err)
			}
		}
	}

	statefulsets, err := h.K8sClient.AppsV1().StatefulSets(h.Config.ReleaseNamespace).List(c, metav1.ListOptions{})
	if err != nil && !errors.IsNotFound(err) {
		return fmt.Errorf("failed to list statefulsets: %v", err)
	}
	for _, statefulset := range statefulsets.Items {
		if statefulset.Annotations["meta.helm.sh/release-name"] == releaseName {
			scale := &autoscalingv1.Scale{
				ObjectMeta: metav1.ObjectMeta{
					Name:      statefulset.Name,
					Namespace: statefulset.Namespace,
				},
				Spec: autoscalingv1.ScaleSpec{
					Replicas: replicas,
				},
			}
			_, err := h.K8sClient.AppsV1().StatefulSets(statefulset.Namespace).UpdateScale(c, statefulset.Name, scale, metav1.UpdateOptions{})
			if err != nil {
				return fmt.Errorf("failed to scale statefulset %s/%s to zero: %v", statefulset.Namespace, statefulset.Name, err)
			}
		}
	}

	return nil
}
