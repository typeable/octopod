# Руководство по установке Octopod

## Варинаты установки
Есть несколько вариантов установки Octopod в зависимости от ваших предпочтений.

### Если у вас есть свободный kubernetes кластер
Для установки Octopod на уже существуюший kubernetes вы можете восмользоваться [Helm чартом](../../charts/octopod).
Чтобы успешно установить Octopod ваш кластер должен соответствовать слежующи требованиям:
- Поддержка PVC
- Установленный Ingress контроллер ([ingress-nginx](https://kubernetes.github.io/ingress-nginx/)) installed
- Версия kubernetes >= 1.19.0

Если кластер соответствует требованиям вы можете воспользоваться [инструкцией](../../charts/octopod/README.md) для установки Octopod с помощью helm чарта.

### Если хочется установить локально
Можно воспользоваться скриптом [octopod_local_install.sh](../../octopod_local_install.sh) для разворачивания Octopod в кластере kind на локальной машине.

Перед запуском скрипта убедитесь, что у вас установлены следующие утилиты:
- [docker](https://docs.docker.com/engine/install/)
- [kind](https://kind.sigs.k8s.io/docs/user/quick-start/#installation)
- [kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)
- [helm 3](https://helm.sh/docs/intro/quickstart/#install-helm)

После установки зависимостей просто запустите скрипт `./octopod_local_install.sh`
В результате работы скрипта у вас будет возможность подлючиться к Octopod на домене `octopod.lvh.me`.
`lvh.me` это специальный домен который всегда резолвится на `127.0.0.1`

## Что дальше?

Сейчас вы, возможно захотите пройти инструкцию по [установке helm чартов](Helm-based_deployment_guide.md) c помощью Octopod.
