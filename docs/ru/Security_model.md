# Security model

<details>
  <summary>Table of contents</summary>

- [Octopod roles](#octopod-roles)
- [Kubernetes role-based access control](#kubernetes-role-based-access-control)
  - [Права на удаление сертификатов](#права-на-удаление-сертификатов)
  - [Права на удаление Persistent Volumes Claims](#права-на-удаление-persistent-volumes-claims)
- [Users auth example with OAuth](#users-auth-example-with-oauth)
- [CLI client Auth](#cli-client-auth)
- [UI Auth](#ui-auth)

</details>

## Octopod roles

В _Octopod_ есть 2 роли:
* _user_
* _admin_

| role  | управление развертываниями | просмотр логов развертываний |
| :---: | :------------------------: | :--------------------------: |
| user  |             ✅              |              ❌               |
| admin |             ✅              |              ✅               |

Пользователи _UI_ работают с правами _user_.

Пользователи _octo CLI_ работают с правами _admin_.

## Kubernetes role-based access control

_Octopod Server_ разворачивается в `octopod` namespace. _Octopod Server_ разворачивает развертывания в `deployments` namespace.
_Octopod Server_ использует `octopod` [_Service Account_][kubernetes-service-account].

Для очистки ресурсов _Octopod Server_ / _Контейнеру с утилитами (control scripts)_ может потребоваться
(зависит от настроект _Kubernetes_ и релизации _Контейнера с утилитами_)
доступ для удаления сертификатов и [_Persistent Volumes Claims_][kubernetes-pvc].

Доступ можно выдать через [_RBAC_][kubernetes-rbac]:

### Права на удаление сертификатов

```yaml
---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: cert-control-clusterrole
rules:
  - apiGroups: ["cert-manager.io"]
    resources: ["certificates"]
    verbs: ["list", "delete", "deletecollection"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: octopod-cert-control-rolebinding
  namespace: deployments
roleRef:
  kind: ClusterRole
  apiGroup: rbac.authorization.k8s.io
  name: cert-control-clusterrole
subjects:
  - kind: ServiceAccount
    name: octopod
    namespace: octopod
```

### Права на удаление Persistent Volumes Claims

```yaml
---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: pvc-control-clusterrole
rules:
  - apiGroups: [""]
    resources: ["persistentvolumeclaims"]
    verbs: ["list", "delete", "deletecollection"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: octopod-pvc-control-rolebinding
  namespace: deployments
roleRef:
  kind: ClusterRole
  apiGroup: rbac.authorization.k8s.io
  name: pvc-control-clusterrole
subjects:
  - kind: ServiceAccount
    name: octopod
    namespace: octopod
```

## Users auth example with OAuth

Доступ к _UI_ по умолчанию не ограничен.
Но вы можете добавить аутентификацию через GitHub OAuth в [_Kubernetes Ingress Nginx_][kubernetes-ingress-nginx-external-auth],
для этого потребуется дополнительно развернуть [_OAuth2 Proxy_][oauth2-proxy] в кластере.

Более подробно описано в примере [_External OAUTH Authentication_][oauth-external-auth].

## CLI client Auth

Для аутентификации _octo CLI_ в _Octopod Server_ используется сертификат.

## UI Auth

Для аутентификации _UI_ в _Octopod Server_ используется Basic Auth.


[kubernetes-service-account]: https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account
[kubernetes-rbac]: https://kubernetes.io/docs/reference/access-authn-authz/rbac
[kubernetes-pvc]: https://kubernetes.io/docs/concepts/storage/persistent-volumes/#expanding-persistent-volumes-claims
[kubernetes-ingress-nginx-external-auth]: https://kubernetes.github.io/ingress-nginx/user-guide/nginx-configuration/annotations/#external-authentication
[oauth2-proxy]: https://oauth2-proxy.github.io/oauth2-proxy
[oauth-external-auth]: https://kubernetes.github.io/ingress-nginx/examples/auth/oauth-external-auth
