# Octopod deployment with K8S

1. Установка необходимых утилит

   Для установки потребуется [_kubectl_][kubectl] и [_helm_][helm] версии 2.

2. Настройка кластера

   _Octopod_ использует [_Ingress Nginx_][ingress-nginx], [_Cert Manager_][cert-manager]. Убедитесь, что они установленны в вашем кластере.

   Для работы _Octopod_ нужны 3 TLS сертификата. [_Cert Manager_][cert-manager] создает сертификаты через [_Let’s Encrypt_][lets-encrypt].
   У [_Let’s Encrypt_][lets-encrypt] есть лимиты на создание сертификатов [_Let’s Encrypt Rate Limits_][lets-encrypt-rate-limits].
   При достижении лимита `too many registrations for this IP` может помочь перемещение Pod [_Cert Manager_][cert-manager] на другую ноду.

   _Octopod_ необходимо не более 2 CPU и 2Gb оперативной памяти, пожалуйста убедитесь что у вас есть необходимые ресурсы.

   _Octopod_ будет развернут на ноду с label `role=stand`, пожалуйста добавте label:
   ```
   kubectl label node <your_node> role=stand
   ```

3. Также вам потребуется [_Tiller_][tiller] – сервис на стороне кластера, который необходим для работы [_helm 2_][helm]. Самый простой способ его установить это:

   ```bash
   helm init
   ```

   Если Вы установили [_Tiller_][tiller], у вас может возникнуть [проблемы с правами доступа](https://github.com/helm/helm/issues/5100) у [_Tiller_][tiller].

   Чтобы выдать ему необходимые права будет достаточно выполнить следующие команды:

   ```bash
   kubectl create -n kube-system serviceaccount tiller
   kubectl --namespace kube-system create clusterrolebinding tiller-cluster-admin --clusterrole=cluster-admin --serviceaccount=kube-system:tiller
   kubectl --namespace kube-system patch deploy tiller-deploy -p '{"spec":{"template":{"spec":{"serviceAccount":"tiller"}}}}'
   ```


4. Скачивание исходного кода проекта

   ```bash
   git clone --branch master https://github.com/typeable/octopod.git /tmp/octopod
   ```

5. Создание namespace `deployment` и `octopod`

   ```bash
   kubectl create namespace deployment
   kubectl create namespace octopod
   ```

6. Создание [_Service Account_][kubernetes-service-account] `octopod`

   ```bash
   kubectl create -n octopod serviceaccount octopod
   ```

7. Создание секрета, который будет использоваться для Basic Auth между _Web UI_ и _Octopod Server_

   ```bash
   username="octopod"
   password="password" # пожалуйста поменяйте на более безопасный пароль
   kubectl create secret generic octopod-basic-auth -n octopod --from-literal=auth=$(htpasswd -bn $username $password)
   ```

8. Создание ConfigMap с сертификатами и ключами, которые будут использоваться для аутентификации между _octo CLI_ и _Octopod Server_

   ```bash
   mkdir certs
   (cd certs && \
   openssl req -x509 -newkey rsa:4096 -keyout server_key.pem -out server_cert.pem -nodes -subj "/CN=localhost/O=Server" && \
   openssl req -newkey rsa:4096 -keyout client_key.pem -out client_csr.pem -nodes -subj "/CN=Client" && \
   openssl x509 -req -in client_csr.pem -CA server_cert.pem -CAkey server_key.pem -out client_cert.pem -set_serial 01 -days 3650)
   kubectl create configmap octopod-certs -n octopod --from-file=./certs
   ```

   После этого директория `certs` будет содержать сертификаты, используемый для авторизации между _octo CLI_ и _Octopod Server_. `client_key.pem` и `client_cert.pem` необходимо будет [через переменные окружения передать в _octo CLI_](Octo_user_guide.md).

9. Далее Вам необходимо будет настроить DNS чтобы поддомены вашего домена указывали на адрес вашего кластера:

   ```
   *.octo.example.com A 1.2.3.4
   ```

10. Так как авторизация между _octo CLI_ и _Octopod Server_ будет производиться через SSL сертификаты, которые мы только что сами создали, нужно чтобы используемый в кластере [_ingress-nginx_][ingress-nginx] поддерживал _SSL passthrough_ – чтобы мы могли ему сказать "не трогай сертификаты".

    Чтобы этого добиться нам необходимо добавить `--enable-ssl-passthrough` в аргументы запуска [_ingress-nginx_][ingress-nginx] контроллера. Сделать это можно с командой вроде этой (конкретный namespace и название деплоймента нужно будет смотреть в вашем конкретном кластере):

    ```bash
    kubectl edit deploy -n ingress-nginx nginx-ingress-ingress-nginx-controller
    ```

    Помимо всех прочих параметров необходимо чтобы там был указан этот:

    ```yaml
    spec:
      ...
      template:
        ...
        spec:
          ...
          containers:
          ...
          - args:
            ...
            - --enable-ssl-passthrough

    ```

11. Установка инфраструктуры Octopod

    Перед началом установки убедитесь, что у вас в кластере есть [_Storage Class_][kubernetes-storage-classes] `default`.
    Для [_minikube_][minikube] вы можете создать [_Storage Class_][kubernetes-storage-classes] `default` так:

    ```bash
    cat <<EOF | kubectl apply -f-
    ---
    apiVersion: storage.k8s.io/v1
    kind: StorageClass
    metadata:
      name: default
    provisioner: k8s.io/minikube-hostpath
    reclaimPolicy: Delete
    volumeBindingMode: Immediate
    EOF
    ```

    ```bash
    cd /tmp/octopod/charts

    helm upgrade --install octopod-infra ./octopod-infra --namespace octopod \
        --wait --timeout 600 --debug
    ```

12. Выдача прав [_Service Account_][kubernetes-service-account] `octopod`

    1. Eсли вы планируете использовать helm версии 2 для развертывания стейджингов,
      то для этого необходимо выдать права [_Service Account_][kubernetes-service-account] `octopod`:
       ```bash
       cd /tmp/octopod/charts
       helm install --name octopod-helm-access ./helm-access
       ```

    2. Eсли вы планируете в `cleanup` из [_Control scripts_](Control_scripts.md) удалять [_Persistent Volumes Claims_][kubernetes-pvc], то для этого необходимо выдать права [_Service Account_][kubernetes-service-account] `octopod`:
       ```bash
       cd /tmp/octopod/charts
       helm install --name octopod-pvc-control ./pvc-control
       ```

    3. Eсли вы используете [_Cert Manager_][cert-manager] и планируете в `cleanup` из [_Control scripts_](Control_scripts.md) удалять сертификаты созданные [_cert-manager_][cert-manager], то для этого необходимо выдать права [_Service Account_][kubernetes-service-account] `octopod`:
       ```bash
       cd /tmp/octopod/charts
       helm install --name octopod-cert-control ./cert-control
       ```

    4. Если Вы планируете использовать [_kubedog_][kubedog] в `check` из [_Control scripts_](Control_scripts.md) для проверки состояния развертывания, то Вам необходимо будет выдать соответствующие права [_Service Account_][kubernetes-service-account] `octopod`:

       ```bash
       cd /tmp/octopod/charts
       helm install --name octopod-kubedog-access ./kubedog-access
       ```

13. Установка сервера Octopod

    ```bash
    cd /tmp/octopod/charts

    # название проекта
    project_name="MyProject"

    # Название вашего docker registry, где находятся docker образы с Octopod Server, octo CLI, Control scripts, sqitch
    registry="typeable"

    # tag Octopod Server
    tag="1.0"

    # название docker образа Octopod Server
    image="octopod"

    # название docker образа _octo CLI_
    octo_image="octo"

    # Название вашего docker registry, где находится docker образ написанными Вами Control Scripts.
    utils_registry="registry_name"

    # название docker образа _Control scripts_
    utils_image="utils"

    # tag _Control scripts_
    utils_image_tag="1.0"

    # email используемый для регистрации в Let’s Encrypt
    acme_registration_email="certbot@example.com"

    # Это не обязательный параметр, если не определить, то доступ к Web UI не будет ограничен аутентификацией.
    # Этот парамет работает в паре с auth_signin, надо либо оба не определять, либо оба определять.
    # Если этот параметр определен, то он передается в ingress-nginx и используется для ограничения доступа через через внешний сервис, например OAuth.
    # Для ограничения доступа через OAuth можно использовать OAuth2 Proxy (Смотри примечание ниже).
    auth_url="https://oauth.exmaple.com/oauth2/auth"

    # Это не обязательный параметр, если не определить, то доступ к Web UI не будет ограничен аутентификацией.
    # Этот парамет работает в паре с auth_url, надо либо оба не определять, либо оба определять.
    # Если этот параметр определен, то он передается в ingress-nginx и используется для ограничения доступа через через внешний сервис, например OAuth.
    # Для ограничения доступа через OAuth можно использовать OAuth2 Proxy (Смотри примечание ниже).
    auth_signin="https://oauth.exmaple.com/oauth2/start?rd=/redirect/$http_host$request_uri"

    # domain по которому будет доступен UI Web
    domain="octo.example.com"

    # домен по которому будет доступен Octopod Server для HTTP запросов
    app_domain="api.octo.example.com"

    # домен по которому будет доступен Octopod Server для WebSocket запросов
    ws_domain="ws.octo.example.com"

    # домен по которому будет доступен Octopod Server для HTTP запросов от octo CLI
    power_app_domain="power.octo.example.com"

    # домен который будет использоваться для создания доменов deployment-ов
    base_domain="octo.example.com"

    # таймаут в секундах после которого обновление состояния deployment считается неудачным
    status_update_timeout=600

    # контрольная сумма chart'а
    # NOTE: на macOS нужно будет `sha256sum` заменить на `shasum -a 256`
    sha256_sum=$(sha256sum octopod/values.yaml octopod/templates/* | awk '{print $1}' | sha256sum | awk '{print $1}')

    # имя пользователя, должно совпадать с именем пользователя из шага 7
    username="octopod"

    # пароль, должен совпадать с паролем из шага 7
    password="password"

    # base64 от имя пользователя и пароля
    base64_of_username_and_password=$(echo -n "$username:$password" | base64)

    helm upgrade --install octopod ./octopod \
       --namespace octopod \
       --set "global.deploy_checksum=$sha256_sum" \
       --set "global.image_prefix=$registry" \
       --set "global.image_tag=$tag" \
       --set "global.image=$image" \
       --set "global.octo_image=$octo_image" \
       --set "global.utils_image_prefix=$utils_registry" \
       --set "global.utils_image=$utils_image" \
       --set "global.utils_image_tag=$utils_image_tag" \
       --set "global.acme_registration_email=$acme_registration_email" \
       --set "global.auth_url=$auth_url" \
       --set "global.auth_signin=$auth_signin" \
       --set "basic_auth_token=$base64_of_username_and_password" \
       --set "project_name=$project_name" \
       --set "domain=$domain" \
       --set "app_domain=$app_domain" \
       --set "ws_domain=$ws_domain" \
       --set "power_app_domain=$power_app_domain" \
       --set "base_domain=$base_domain" \
       --set "status_update_timeout=$status_update_timeout" \
       --wait --timeout 600 --debug
    ```

**Примечание**: Более подробно про настройку OAuth описано в [_Security model_](Security_model.md#users-auth-example-with-oauth)

[kubectl]: https://kubernetes.io/docs/tasks/tools/install-kubectl
[helm]: https://v2.helm.sh/docs/using_helm/#installing-helm
[ingress-nginx]: https://kubernetes.github.io/ingress-nginx
[cert-manager]: https://cert-manager.io/docs
[lets-encrypt]: https://letsencrypt.org
[lets-encrypt-rate-limits]: https://letsencrypt.org/docs/rate-limits
[kubernetes-service-account]: https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account
[kubernetes-pvc]: https://kubernetes.io/docs/concepts/storage/persistent-volumes/#expanding-persistent-volumes-claims
[kubernetes-storage-classes]: https://kubernetes.io/docs/concepts/storage/storage-classes
[minikube]: https://kubernetes.io/ru/docs/tasks/tools/install-minikube
[tiller]: https://v2.helm.sh/docs/install
[kubedog]: https://github.com/werf/kubedog
