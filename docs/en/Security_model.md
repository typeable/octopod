# Security model

<details>
  <summary>Table of contents</summary>

- [Octopod roles](#octopod-roles)
- [Kubernetes role-based access control](#kubernetes-role-based-access-control)
  - [Privileges to delete certificates](#privileges-to-delete-certificates)
  - [Privileges to delete _Persistent Volumes Claims_](#privileges-to-delete-persistent-volumes-claims)
- [Web UI authentication](#web-ui-authentication)
- [Web UI OAuth](#web-ui-oauth)
- [octo CLI authentication](#octo-cli-authentication)

</details>

## Octopod roles

There are two user roles in _Octopod_:
* _user_
* _admin_

| role  | managing deployments | viewing deployment logs |
| :---: | :------------------: | :---------------------: |
| user  |          ✅           |            ❌            |
| admin |          ✅           |            ✅            |

_Web UI_ users have the _user_ role.

_octo CLI_ users have the _admin_ role.

There is currently no way to give someone access to _octo CLI_ without giving them the _admin_ role since authentication is done through SSL certificates instead of through OAuth.

## Kubernetes role-based access control

_Octopod Server_ is deployed in the `octopod` _Kubernetes_ namespace. Deployments are deployed in the `deployments` namespace.
_Octopod Server_ uses the `octopod` [_Service Account_][kubernetes-service-account].

Octopod needs a sufficient permissions to run helm inside kubernetes and create all resources described in helm chart it is installing. Thus permissions are quite extended.
RBAC rules to describe permissions needed are added automatically by the octopod helm chart and can be viewed [here](../../charts/helm3/octopod/templates/rbac.yaml)

## Web UI authentication

Authentication between the _Web UI_ and _Octopod Server_ is done through _Basic Auth_. The _Bearer token_ is read by the _Web UI_ after the page is loaded as part of [the config](../../charts/octopod/templates/octopod-nginx-configmap.yaml#L15-L20). By default, everything, including the config, can be accessed without any authentication. For ways of mitigating this please see the next section.

## Web UI OAuth

The [_Web UI_](Technical_architecture.md#-web-ui) on its own does not have any authentication whatsoever, meaning that anyone can open it and manage your deployments. Luckily, _Kubernetes_ [can be configured](../../charts/octopod/templates/octopod-ingress.yaml#L15-L21) to authenticate users before they get access to the _Web UI_. It can be set up to authenticate users through [_Ingress_](https://kubernetes.io/docs/concepts/services-networking/ingress/) which [supports external authentication services][kubernetes-ingress-nginx-external-auth]. You can set up [_OAuth2 Proxy_][oauth2-proxy] in your cluster to support numerous OAuth services. For example, if you use GitHub, you can set up [_OAuth2 Proxy_][oauth2-proxy] to use GitHub to automatically grant users access to Octopod when you add them to your organization in GitHub.

## octo CLI authentication

Authentication between _octo CLI_ and _Octopod Server_ is done through special token which is generated automatically or specified by user in `octopod.cliAuthSecret` parameter, as described [here](../../charts/helm3/octopod#parameters)

[kubernetes-service-account]: https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account
[kubernetes-rbac]: https://kubernetes.io/docs/reference/access-authn-authz/rbac
[kubernetes-pvc]: https://kubernetes.io/docs/concepts/storage/persistent-volumes/#expanding-persistent-volumes-claims
[kubernetes-ingress-nginx-external-auth]: https://kubernetes.github.io/ingress-nginx/user-guide/nginx-configuration/annotations/#external-authentication
[oauth2-proxy]: https://oauth2-proxy.github.io/oauth2-proxy
