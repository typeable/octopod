# Technical architecture

<details>
  <summary>Table of contents</summary>

- [Used tools](#used-tools)
- [App architecture](#app-architecture)
  - [UI](#ui)
  - [DMS](#dms)
  - [PostgreSQL](#postgresql)
  - [DMC](#dmc)
  - [Контейнер с утилитами (app wrappers)](#контейнер-с-утилитами-app-wrappers)
  - [Clean Archive CronJob](#clean-archive-cronjob)
  - [Kube API Server](#kube-api-server)
- [Files of app wrappers](#files-of-app-wrappers)
  - [create](#create)
  - [update](#update)
  - [delete](#delete)
  - [check](#check)
  - [cleanup](#cleanup)
  - [archive_check](#archive_check)
- [DM Distribution model](#dm-distribution-model)
- [App distribution model. **TODO: Deployment guide link.**](#app-distribution-model-todo-deployment-guide-link)
- [Process view](#process-view)
  - [Create](#create-1)
    - [Create via CLI](#create-via-cli)
    - [Create via UI](#create-via-ui)
  - [Update](#update-1)
    - [Update via CLI](#update-via-cli)
    - [Update via UI](#update-via-ui)
  - [Delete](#delete-1)
    - [Delete via CLI](#delete-via-cli)
    - [Delete via UI](#delete-via-ui)
  - [Cleanup](#cleanup-1)
    - [Ceanup via CLI](#ceanup-via-cli)
    - [Ceanup via UI](#ceanup-via-ui)
  - [Restore](#restore)
    - [Restore via CLI](#restore-via-cli)
    - [Restore via UI](#restore-via-ui)
- [How we use it](#how-we-use-it)
- [**TODO: Security model link**](#todo-security-model-link)
- [Per staging statuses and status changes](#per-staging-statuses-and-status-changes)

</details>

## Used tools

_DM_ разворачивается в [_Kubernetes_](kube) и используется для разворачивания stagings в [_Kubernetes_](kube).

Для хранения настроек и логов действий пользователей используется [_PostgreSQL_](#postgresql).

## App architecture

![App architecture](diagrams/images/app-architecture.png)

### UI

_UI_ – веб интерфейс,
используется для управления стейджингами посредством отправки команд управления стейджингами.
Взаимодействует с [_DMS_](#dms) посредством отправки HTTP/1.1 запросов, а также получает
события от [_DMS_](#dms) по _Websocket_. Между _UI_ и [_DMS_](#dms) Basic Auth. Написан на Haskell и Reflex-Dom.

### DMS

_DMS_ – сервер обрабатывает
команды управления стейджингами от [_DMC_](#dmc) и [_UI_](#ui), обновляет состояние стейджингов.
[_DMC_](#dmc) и [_UI_](#ui) взаимодействуют с ним путем отправки HTTP/1.1 запросов.
Сервер шлет событие обновления на [_UI_](#ui) по _Websocket_. Сервер взаимодействует с [_Kube API Server_](#kube-api-server)
через [_контейнер с утилитами (app wrappers)_](#контейнер-с-утилитами-app-wrappers). Для хранения настроек и статусов стейджингов,
и логов действий пользователей используется [_PostgreSQL_](#postgresql). Написан на Haskell. Для работы
со стейджингами использует [_контейнер с утилитами (app wrappers)_](#контейнер-с-утилитами-app-wrappers).

### PostgreSQL

[_PostgreSQL_](https://www.postgresql.org) – РСУБД для хранения настроек и статусов стейджингов, и логов
действий пользователей.

### DMC

_DMC_ – консольный клиент
используется для управления стейджингами посредством отправки команд управления
стейджингами. Взаимодействует с [_DMS_](#dms) посредством отправки HTTP/1.1 запросов.
Написан на Haskell. Между _DMC_ и [_DMS_](#dms) аутентификация по сертификату,
новый сертификаты создается вовремя каждой сборки и упаковываются в контейнеры с _DMC_ и [_DMS_](#dms).

### Контейнер с утилитами (app wrappers)

_Контейнер с утилитами (app wrappers)_ – контейнер с исполнямыми файлами, в которые инкапсулирована
логика работы с [_Kube API Server_](#kube-api-server), облачными провайдерами, стейджингами, системами контроля версий и т.д.
Во время старта пода [_DMS_](#dms) содержимое контейнера с утилитами копируется в ФС контейнера с [_DMS_](#dms),
поэтому исполняемые файлы должны быть либо интерпретируемы через _Bash_, либо статически слинкованны.
Логику исполняемых файлов предлагается реализовать пользователям _DM_ (смотри раздел [_Files of app wrappers_](#files-of-app-wrappers)).

### Clean Archive CronJob
_Clean Archive CronJob_ – CronJob, которая запускается раз в час и через [_DMC_](#dmc) удаляет заархивированные стейджинги старее 14 дней.

### Kube API Server

[Kube API Server](https://kubernetes.io/docs/concepts/overview/kubernetes-api/) – сервер обрабатывающий HTTP/1.1 запросы. Точка входа для
взаимодействия с [_Kubernetes_](kube).

## Files of app wrappers

### create

Реализация создания нового стейджинга.

Получает на вход следующие аргументы:
* `--project-name` – название проекта
* `--base-domain` – базовый домен
* `--namespace` – namespace
* `--name` – имя стейджинга
* `--tag` – тег стейджинга
* `--app-env-override` – override уровня приложения (формат значения `FOO=BAR`, может быть передано 0 или более раз)
* `--staging-override` – override уровня стейджинга (формат значения `FOO=BAR`, может быть передано 0 или более раз)

Успешность операции определяется по exit code (`0` – успех).

### update

Реализация обновления стейджинга.

Получает на вход следующие аргументы:
* `--project-name` – название проекта
* `--base-domain` – базовый домен
* `--namespace` – namespace
* `--name` – имя стейджинга
* `--tag` – тег стейджинга
* `--app-env-override` – override уровня приложения (формат значения `FOO=BAR`, может быть передано 0 или более раз)
* `--staging-override` – override уровня стейджинга (формат значения `FOO=BAR`, может быть передано 0 или более раз)

Успешность операции определяется по exit code (`0` – успех).

### delete

Реализация удаления стейджинга.

Получает на вход следующие аргументы:
* `--project-name` – название проекта
* `--namespace` – namespace
* `--name` – имя стейджинга

Успешность операции определяется по exit code (`0` – успех).

### check

Реализация проверки состояния стейджинга.

Получает на вход следующие аргументы:
* `--namespace` – namespace
* `--name` – имя стейджинга

Успешность операции определяется по exit code (`0` – успех).

### cleanup

Реализация очистки ресурсов стейджинга.

Получает на вход следующие аргументы:
* `--project-name` – название проекта
* `--namespace` – namespace
* `--name` – имя стейджинга

Успешность операции определяется по exit code (`0` – успех).

### archive_check

Реализация проверки успешности удаления стейджинга.

Получает на вход следующие аргументы:
* `--namespace` – namespace
* `--name` – имя стейджинга

Успешность операции определяется по exit code (`0` – успех).

## DM Distribution model

[_DMC_](#dmc) поставляется упаковынным в Docker образ или в виде статически слинкованного файла (не решили как лучше).

[_DMS_](#dms) и [_UI_](#ui) поставляются упаковынными в один Docker образ. Для развертывания в [_Kubernetes_](kube) используется набор [_Chart_](chart)-ов.

Docker образ [_контейнера с утилитами (app wrappers)_](#контейнер-с-утилитами-app-wrappers) пользователь _DM_ собирает сам.

## App distribution model. **TODO: Deployment guide link.**

## Process view

### Create

_create_ – создание нового стейджинга.
  В качестве аргументов принимаются `name`, `tag` и опциональные `overrides` (уровня App или Staging, открытие или секретные).

#### Create via CLI
![Create](diagrams/images/technical-architecture-create-via-cli.png)

#### Create via UI
![Create](diagrams/images/technical-architecture-create-via-ui.png)

### Update

_update_ – обновление существующего стейджинга.
  В качестве аргументов принимаются `name`, `tag` и опциональные `overrides` (уровня App или Staging, открытие или секретные).

#### Update via CLI
![Update](diagrams/images/technical-architecture-update-via-cli.png)

#### Update via UI
![Update](diagrams/images/technical-architecture-update-via-ui.png)

### Delete

_delete_ – архивирование существующего стейджига.
  Производится удаление только подов, Persistent Volumes (диски) сохраняются. Отменить действие этой команды можно с помощью команды restore.
  В качестве аргументов принимаются `name`.

#### Delete via CLI
![Delete](diagrams/images/technical-architecture-delete-via-cli.png)

#### Delete via UI
![Delete](diagrams/images/technical-architecture-delete-via-ui.png)

### Cleanup

_cleanup_ – полная очистка стейджинга.
  Удаление сертификатов, Persistent Volume Claim и Persistent Volumes.
  В качестве аргументов принимаются `name`.

#### Ceanup via CLI
![Cleanup](diagrams/images/technical-architecture-cleanup-via-cli.png)

#### Ceanup via UI
![Cleanup](diagrams/images/technical-architecture-cleanup-via-ui.png)

### Restore

_restore_ – восстановление заархивированного стейджинга.
  Восстановление заархивированного стейджинга с последними настройками.
  В качестве аргументов принимаются `name`.

#### Restore via CLI
![Restore](diagrams/images/technical-architecture-restore-via-cli.png)

#### Restore via UI
![Restore](diagrams/images/technical-architecture-restore-via-ui.png)

## How we use it

Мы используем несколько кластеров [_Kubernetes_](kube): отдельный кластер для каждого приложения, а так же разделяем по кластерам _production_ и _staging_ окружения.

В каждый _staging_ кластер мы устанавливаем _DM_, через _DM_ осуществляем разворачивание различных версий staging'ов необходимых для QA.

## **TODO: Security model link**

## Per staging statuses and status changes

Существует 6 статусов стейджигов:
1. *Running*
2. *Failure*
3. *CreatePending*
4. *UpdatePending*
5. *DeletePending*
6. *Archived*

_Running_, _Failure_, _Archived_ являются постоянными, т.е. стейджинг уже не находится в режиме выполнения команды.

*CreatePending*, *UpdatePending*, *DeletePending* являются переходными, т.е. стейджинг находится в режиме выполнения команды.

![Staging Statuses](diagrams/images/technical-architecture-staging-statuses-fsm.png)

[kubectl]: https://kubernetes.io/docs/reference/kubectl/
[helm]: https://helm.sh
[kubedog]: https://github.com/werf/kubedog
[kube]: https://kubernetes.io
[chart]: https://helm.sh/docs/topics/charts/
