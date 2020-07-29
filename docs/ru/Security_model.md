# Security model

<details>
  <summary>Table of contents</summary>

- [DM roles](#dm-roles)
- [K8S RBAC](#k8s-rbac)
  - [Права на удаление сертификатов](#права-на-удаление-сертификатов)
  - [Права на удаление Persistent Volumes Claims](#права-на-удаление-persistent-volumes-claims)
- [Users auth example with OAuth. **TODO: link to DM chart**](#user-auth-example-with-oauth)
- [CLI client Auth. **TODO: поправить с учетом способа поставки DMC**](#cli-client-auth)
- [UI Auth. **TODO: link to DM chart**](#ui-auth)

</details>

## DM roles

В _DM_ есть 2 роли:
* _user_
* _admin_

| role | управление стеджингами (включая обновление секретных override) | просмотр секретных override | просмотр логов деплоя |
| :---: | :---: | :---: | :---: |
| user | + | - | - |
| admin | + | + | + |

Пользователи _UI_ работают с правами _user_.

Пользователи _DMC_ работают с правами _admin_.

## K8S RBAC

_DMS_ разворачивается в `dm` namespace. _DMS_ разворачивает стейджинги в `staging` namespace.
_DMS_ использует `dm` [_Serivce Account_][kubernetes-service-account].

Для очистки ресурсов _DMS_ / _Контейнеру с утилитами (staging control scripts)_ может потребоваться
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
  name: dm-cert-control-rolebinding
  namespace: staging
roleRef:
  kind: ClusterRole
  apiGroup: rbac.authorization.k8s.io
  name: cert-control-clusterrole
subjects:
  - kind: ServiceAccount
    name: dm
    namespace: dm
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
  name: dm-pvc-control-rolebinding
  namespace: staging
roleRef:
  kind: ClusterRole
  apiGroup: rbac.authorization.k8s.io
  name: pvc-control-clusterrole
subjects:
  - kind: ServiceAccount
    name: dm
    namespace: dm
```

## Users auth example with OAuth. **TODO: link to DM chart**

Доступ к _UI_ ограничен через OAuth. OAuth настроен на [_Kubernetes Ingress Nginx_][kubernetes-ingress-nginx-external-auth] [**TODO: link to DM chart**].

## CLI client Auth. **TODO: поправить с учетом способа поставки DMC**

Для аутентификации _DMC_ в _DMS_ используется сертификат.
Но возможно и Basic Auth (зависит от способа поставки _DMC_, не приняли окончательного решения) (**TODO: поправить с учетом способа поставки DMC**).

## UI Auth. **TODO: link to DM chart**

Для аутентификации _UI_ в _DMS_ используется Basic Auth [**TODO: link to DM chart**]


[kubernetes-service-account]: https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account
[kubernetes-rbac]: https://kubernetes.io/docs/reference/access-authn-authz/rbac
[kubernetes-pvc]: https://kubernetes.io/docs/concepts/storage/persistent-volumes/#expanding-persistent-volumes-claims
[kubernetes-ingress-nginx-external-auth]: https://kubernetes.github.io/ingress-nginx/user-guide/nginx-configuration/annotations/#external-authentication
