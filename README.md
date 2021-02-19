<p align="center">
   <img src="img/logo.svg" width="350"></img>
</p>


<p align="center">

<a href="https://github.com/typeable/octopod/actions?query=workflow%3ABuild"><img src="https://github.com/typeable/octopod/workflows/Build/badge.svg?branch=master"></a>

</p>

   [![Build](https://github.com/typeable/octopod/workflows/Build/badge.svg?branch=master)](https://github.com/typeable/octopod/actions?query=workflow%3ABuild+branch%3Amaster) [![Documentation](https://github.com/typeable/octopod/workflows/Documentation/badge.svg?branch=master)](https://github.com/typeable/octopod/actions?query=workflow%3ADocumentation+branch%3Amaster)

_Octopod_ is a fully open-source self-hosted solution for managing multiple deployments in a _Kubernetes_ cluster with a user-friendly web interface. Managing deployments does not require any technical expertise.

We created _Octopod_ because we believe that everything we release should be rigorously tested, however, such desires greatly [complicate the development workflow](docs/en/PM_case_study.md) leading to longer release cycles. We use _Octopod_ to mitigate the downsides of rigorously testing each feature by deploying every single change we make to a separate staging environment allowing QA to investigate each feature independently and in parallel.

## 🖥 Demo

<p align="center"><img src="img/demo.gif"></img></p>

## 📑 Documentation

### 🔭 High-level notes
- [🐙 Overview](docs/en/Overview.md)
- [🧑‍🔬 Project management case study](docs/en/PM_case_study.md)
- [🧑‍💻 Technical case study](docs/en/Tech_case_study.md)

### 🛠️ Technical documentation
- [🏗 Technical architecture](docs/en/Technical_architecture.md) [[RU](docs/ru/Technical_architecture.md)]
- [⚙️ Control script guide][cs] [[RU](docs/ru/Control_scripts.md)]
- [🔧🐙 Octopod deployment guide](docs/en/Octopod_deployment_guide.md) [[RU](docs/ru/Octopod_deployment_with_K8S.md)]
- [🔧🚀 Helm-based Octopod project setup](docs/en/Helm-based_deployment_guide.md) [[RU](docs/ru/Helm-based_deployment_guide.md)]
- [🐙🎛 octo CLI user guide][octo]  [[RU](docs/ru/Octo_user_guide.md)]
- [🤖 CI integration](docs/en/Integration.md)
- [🔒 Octopod security model](docs/en/Security_model.md)  [[RU](docs/ru/Security_model.md)]

## ℹ️ FAQ

### How long does it take to set up _Octopod_?

The longest part of setting up _Octopod_ for your project will probably be writing [_Control Scripts_][cs]. In total you should be able to get things running in about a day.

### Will _Octopod_ work with my project if it uses X?

Yes. _Octopod_ is project-agnostic. If you can run your project in a Docker container, then you can use _Octopod_ with that project.

### What do I need to know to set up Octopod?

You need to understand the basics of _Kubernetes_ and be familiar with whatever hosting provider you will be using. There is no need to know any special language – you can write [_Control Scripts_][cs] in whatever language you like.

### Does _Octopod_ work with my CI?

Yes. If you can run arbitrary executables in your CI, then you will be able to integrate it with _Octopod_. Integration basically consists of calling our _octo CLI_ tool to perform desired actions. You can find more detail in the [CI integration](docs/en/Integration.md) doc.

### How come I can't see the deployment logs in Octopod web app?

It's been excluded from the GUI because we don't have a good security story to accompany this feature yet. Some secrets and credentials may leak to the project team using Octopod and, potentially, not everyone should have access to this data.

### Octopod says "Failure" next to my deployment. Why is it not working?

There are several places where things can go wrong:

1. Your DevOps engineer could have made mistakes in one of the [_Control Scripts_][cs] leading to an improper deployment process.

   To fix this the DevOps engineer can use the [_octo CLI_][octo] to see detailed logs collected during the deployment process and diagnose the issue.
2. You could have one of the steps of your deployment failing (for example, a failing database migration).

   If you have supplied a [_Kubernetes Dashboard_](https://kubernetes.io/docs/tasks/access-application-cluster/web-ui-dashboard/) URL to _Octopod_ during deployment, then you will have a "Details" button in the _Web UI_. You can click that button to get a filtered view of the _Kubernetes Dashboard_ where you can see what could have gone wrong and diagnose the issue.

### Why Haskell and Rust?

We believe that there is a lot to be gained in programming in general by being able to statically ensure invariants in your code. One of the most practical ways of ensuring invariants is a good static type system. Haskell and Rust are both languages that have very strong type systems. This allows us to move fast without breaking things in the process.

## Quotations

> Typeable team envisioned an insightful approach to CI/CD, which managed to help us cut the delivery time for new features and projects with Octopod

— Wes Iwanski – VP Technology, Downtown Travel

## ❓ Still have questions?

If you still have questions, be sure to ask them in our [Octopod Discussions](https://github.com/typeable/octopod/discussions).

<p align="center"><a href="https://typeable.io"><img src="img/typeable.png" width="177px"></img></a></p>

[cs]: docs/en/Control_scripts.md
[octo]: docs/en/Octo_user_guide.md
