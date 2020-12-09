# Octo user guide

_octo CLI_ – консольная утилита для управления deployments через отправку команд в _Octopod Server_.

## Environment variables

`OCTOPOD_URL` – URL к _Octopod Server_, например `https://octopod-power-app.example.com:443`, **необходимо определить, иначе _octo CLI_ не будет работать**.

`TLS_CERT_PATH` – путь к файлу сертификата, который используется для аутентификации в _Octopod Server_, если не определенно, то используется `./cert.pem`.

`TLS_KEY_PATH` – путь к файлу ключа, который используется для аутентификации в _Octopod Server_, если не определенно, то используется `./key.pem`.

## Subcommands

### create

Создать новый deployment.

Аргументы:

* `-n` или `--name` – название deployment
* `-t` или `--tag` – тег deployment
* `-e` или `--set-app-env-override` – override уровна приложения, формат: КЛЮЧ=ЗНАЧЕНИЕ, можно использовать 0 и более раз
* `-o` или `--set-deployment-override` – override увроня deployment, формат: КЛЮЧ=ЗНАЧЕНИЕ, можно использовать 0 и более раз

Пример:

```bash
$ octo create -n hello-octopod -t ca5fd1fe08389f6422a506a59b68a5272ac37ba6 -e KEY1=VALUE1 -e KEY2=VALUE2
```

### list

Получить список всех deployments.

Данная subcommand не требует передачи аргументов.

Пример:

```bash
$ octo list
hello-octopod
```

### archive

Удалить deployment.

Аргументы:

* `-n` или `--name` – название deployment

Пример:

```bash
$ octo archive -n hello-octopod
```

### update

Обновить deployment.

Аргументы:

* `-n` или `--name` – название deployment
* `-t` или `--tag` – тег deployment
* `-e` или `--set-app-env-override` – override уровна приложения, формат: КЛЮЧ=ЗНАЧЕНИЕ, можно использовать 0 и более раз
* `-E` или `--unset-app-env-override` – override уровна приложения, которое надо удалить, формат: КЛЮЧ, можно использовать 0 и более раз
* `-o` или `--set-deployment-override` – override увроня deployment, формат: КЛЮЧ=ЗНАЧЕНИЕ, можно использовать 0 и более раз
* `-O` или `--unset-deployment-override` – override увроня deployment, которое надо удалить, формат: КЛЮЧ, можно использовать 0 и более раз

Пример:

```bash
$ octo update -n octopod -t 015f16ecf398fcadaac508c1855ae160af0969c4 -E KEY1 -e KEY2=VALUE22222
```

### info

Получить информацию о deployment.

Аргументы:

* `-n` или `--name` – название deployment

Пример:

```bash
$ octo info -n hello-octopod
Current settings:
tag: v1
application overrides: app=1 (Public)

deployment overrides: dep=2 (Public)

metadata:
  app: https://ree.lvh.me

Last logs:
┏━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━┳━━━━━━━━┳━━━━━┳━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━┓
┃     Created at      ┃ Action id ┃ Action ┃ Tag ┃ App overrides  ┃ Deployment overrides ┃ Exit code ┃
┡━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━╇━━━━━━━━╇━━━━━╇━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━┩
│ 2020-11-02T17:14:03 │     7     │ create │ v1  │ app=1 (Public) │ dep=2 (Public)       │     1     │
├─────────────────────┼───────────┼────────┼─────┼────────────────┼──────────────────────┼───────────┤
│ 2020-11-02T19:01:02 │     8     │ update │ v1  │ app=1 (Public) │ dep=2 (Public)       │     1     │
└─────────────────────┴───────────┴────────┴─────┴────────────────┴──────────────────────┴───────────┘
```

### cleanup

Очистить ресурсы от deployment.

Аргументы:

* `-n` или `--name` – название deployment

Пример:

```bash
$ octo cleanup -n hello-octopod
```

### restore

Восстановить deployment.

Аргументы:

* `-n` или `--name` – название deployment

Пример:

```bash
$ octo restore -n hello-octopod
```

### clean-archive

Удалить архивные deployments старее 2х недель.

Данная subcommand не требует передачи аргументов.

Пример:

```bash
$ octo clean-archive
```

### logs

Печатает логи выбранного деплоя.

Аргументы:

* `-a` или `--action` – action id, который вернулся из команды `octo info`.
* `-l` или `--log-type` – Какие логи необходимо вернуть. Значения: `stdout`, `stderr`, `all`. По умолчанию будет `all`

Пример:

```
$ octo logs -a 13
		stdout:


		stderr:

error: Found argument '--deployment-override' which wasn't expected, or isn't valid in this context

USAGE:
    update --app-env-override <app-env-override>... --base-domain <base-domain> --name <name> --namespace <namespace> --project-name <project-name> --tag <tag>

For more information try --help
```
