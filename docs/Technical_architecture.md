### Technical architecture

#### Used tools(K8s, kubectl, helm, postgresql, kubedog)

DM сам разворачивается в Kubernetes, а так же разворачивает новые stagings в Kubernetes.

Для управления и шаблонизации stagings используется Kubectl и Helm.

Для проверки состояния стейджингов используется Kubedog.

Для хранения настроек и логов действий пользователей используется PostgreSQL.

#### App architecture: UI, server, client, app wrappers, cronjob, database

![App architecture](diagrams/images/app-architecture.png)

UI (https://github.com/Aviora/dm/tree/master/dm-frontend) - веб интерфейс,
используется для управления стейджингами посредством отправки команд управления стейджингами.
Взаимодействует с DMS посредством отправки HTTP/1.1 запросов, а так же получает
события от DMS по Websocket. Между UI и DMS Basic Auth. Написан на Haskell и Reflex-Dom.

DMS (https://github.com/Aviora/dm/tree/master/dm-backend) - сервер обрабатывает
команды управления стейджингами от DMC и UI, обновляет состояние стейджингов.
DMC и UI взаимодействуют с ним путем отправки HTTP/1.1 запросов.
Сервер шлет событие обновления на UI по Websocket. Сервер взаимодействует с Kube API Server
через контейнер с утилитами (app wrappers).  Для хранения настроек и статусов стейджингов,
и логов действий пользователей используется PostgreSQL. Написан на Haskell. Для работы
со стейджингами использует контейнер с утилитами (app wrappers).

PostgreSQL - РСУБД для хранения настроек и статусов стейджингов, и логов
действий пользователей.

DMC (https://github.com/Aviora/dm/tree/master/dm-backend) - консольный клиент
используется для управления стейджингами посредством отправки команд управления
стейджингами. Взаимодействует с DMS посредством отправки HTTP/1.1 запросов.
Написан на Haskell. Между DMC и DMS аутентификация по сертификату,
новый сертификаты создается вовремя каждой сборки и упаковываются в контейнеры с DMC и DMS.

Контейнер с утилитами (app wrappers) (https://github.com/Aviora/dm/tree/master/utils/b2b-utils) -
набор статически слинкованных или интерпретируемых исполняемых файлов (create
/ update / delete / check / cleanup / archive_check). Для работы с конкретным типом стейджинга
(например b2b) и с помощью конкретного набора утилит (helm, kubectl, kubedog),
которые взаимодействуют с Kubernetes посредством отправки запросов в Kube API
Server. Сhart-ы разворачиваемого стейджинга находятся git-репозитории
https://github.com/Aviora/b2b-helm, настроен доступ к этому репозиторию из контейнера DMS:
настраивается во время сборки Docker образа DMS (есть планы перенести подобную инициализацию в исполняемый файл init).
Во время старта пода DMS содержимое контейнера с утилитами копируется в ФС контейнера с DMS,
отсюда возникает ограничение на реализацию утилит:
либо то, что можно интерпретировать с помощью окружения DMS (например на Bash), либо
статически слинкованный бинарники (например на Rust). Так же есть контейнер с
утилитами с замоканым Kubernetes (https://github.com/Aviora/dm/tree/master/utils/mock-utils/docker).
Контейнер с утилитами для b2b написан на Rust, с замоканым Kubernetes - на Bash.

Kubernetes (https://kubernetes.io/docs/home/) - распределенная система для
оркестрации контейнерами.

Kube API Server - сервер обрабатывающий HTTP/1.1 запросы. Точка входа для
взаимодействия с Kubernetes.

#### DM Distribution model

В результате сборки проекта в CI получаем 2 Docker образа:
1. Образ с консольным клиентом DMC
2. Образ с сервером DMS и UI

Каждый образ заливается в свой Docker репозиторий.

Во время сборки в Docker образы DMC и DMS добавляются сертификаты для меж сервисного взаимодействия.

Во время сборки в Docker образ DMS добавляется Deploy Key для доступа к https://github.com/Aviora/b2b-helm.

Образ контейнера с утилитами (app wrappers) собирается отдельно.

Docker образы с сервером DMS и UI, и контейнером с утилитами (app wrappers) разворачиваются в Kubernetes.


#### App distribution model. Deployment guide link.

#### Primary processes(create, update, archive - describe relation with stateful sets, restore, cleanup)

Команды управления стейджингами:
* create - создание нового стейджинга.
  В качестве аргументов принимаются `name`, `tag` и опциональные `overrides` (уровня App или Staging, открытие или секретные).
* update - обновление существующего стейджинга.
  В качестве аргументов принимаются `name`, `tag` и опциональные `overrides` (уровня App или Staging, открытие или секретные). 
* delete - архивирование существующего стейджига.
  Производится удаление только подов, Persistent Volumes (диски) сохраняются. Отменить действие этой команды можно с помощью команды restore.
  В качестве аргументов принимаются `name`. 
* cleanup - полная очистка стейджинга.
  Удаление сертификатов, Persistent Volume Claim и Persistent Volumes.
  В качестве аргументов принимаются `name`. 
* restore — восстановление заархивированного стейджинга.
  Восстановление заархивированного стейджинга с последними настройками.
  В качестве аргументов принимаются `name`. 

#### Synchronous and Asynchronous process diagrams

##### Create via CLI
![Create](diagrams/images/technical-architecture-create-via-cli.png)

##### Create via UI
![Create](diagrams/images/technical-architecture-create-via-ui.png)

##### Update via CLI
![Update](diagrams/images/technical-architecture-update-via-cli.png)

##### Update via UI
![Update](diagrams/images/technical-architecture-update-via-ui.png)

##### Delete via CLI
![Delete](diagrams/images/technical-architecture-delete-via-cli.png)

##### Delete via UI
![Delete](diagrams/images/technical-architecture-delete-via-ui.png)

##### Ceanup via CLI
![Cleanup](diagrams/images/technical-architecture-cleanup-via-cli.png)

##### Ceanup via UI
![Cleanup](diagrams/images/technical-architecture-cleanup-via-ui.png)

##### Restore via CLI
![Restore](diagrams/images/technical-architecture-restore-via-cli.png)

##### Restore via UI
![Restore](diagrams/images/technical-architecture-restore-via-ui.png)

#### How we use it(B2B staging cluster, no mention of the project itself)

Мы используем несколько кластеров Kubernetes: отдельный кластер для каждого приложения, а так же разделяем по кластерам prod и staging окружения.

В каждый staging кластер мы устанавливаем DM, через DM осуществляем разворачивание различных версий staging'ов необходимых для QA.

#### Security model link

#### Per staging statuses and status changes

Существует 6 статусов стейджигов:
1. Running
2. Failure
3. CreatePending
4. UpdatePending
5. DeletePending
6. Archived

Running, Failure, Archived являются постоянными, т.е. стейджинг уже не находится в режиме выполнения команды.

CreatePending, UpdatePending, DeletePending являются переходными, т.е. стейджинг находится в режиме выполнения команды.

![Staging Statuses](diagrams/images/technical-architecture-staging-statuses-fsm.png)
