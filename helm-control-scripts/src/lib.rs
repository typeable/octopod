pub mod lib {
    pub use std::io::prelude::*;
    pub use std::process::{Command, ExitStatus, Stdio};
    pub use std::error::Error;
    pub use std::fmt;
    pub use structopt::StructOpt;
    pub use env_logger::{Builder, Target};
    pub use log::{LevelFilter, info, warn, error};
    pub use serde::{Serialize, Deserialize};
    pub use serde_json::json;
    pub use std::error;
    pub use yaml_rust::{YamlLoader, YamlEmitter, Yaml};
    pub use k8s_openapi::api::{
        apps::v1::{Deployment, StatefulSet},
        networking::v1::Ingress,
        networking::v1beta1::Ingress as OldIngress,
        core::v1::{PersistentVolumeClaim, Secret}
    };
    pub use kube::{
        api::{Api, ListParams, ResourceExt, Patch, PatchParams, DeleteParams, PostParams},
        Client,CustomResource
    };
    pub use dkregistry::{
        v2::Client as RegClient,
        errors::Error as RegError,
    };
    pub use regex::Regex;
    pub use rusoto_core::{Region, RusotoError};
    pub use rusoto_ecr::{Ecr, EcrClient, DescribeImagesRequest, ImageIdentifier, DescribeImagesError};
    pub use schemars::JsonSchema;
    pub use tokio::task;

    #[derive(Debug,StructOpt)]
    #[structopt(rename_all = "kebab-case")]
    pub struct CliOpts {
        #[structopt(long)]
        pub project_name: String,
        #[structopt(long)]
        pub base_domain: String,
        #[structopt(long)]
        pub namespace: String,
        #[structopt(long)]
        pub name: Option<String>,
        #[structopt(long)]
        pub tag: Option<String>,
        #[structopt(long)]
        pub application_config: Vec<String>,
        #[structopt(long)]
        pub deployment_config: Vec<String>,
    }

    #[derive(Deserialize, Debug)]
    pub struct EnvVars {
        pub helm_bin: String,
        pub defaults: String, //json of DefaultValues
        #[serde(default)]
        pub helm_user: String,
        #[serde(default)]
        pub helm_pass: String,
        pub helm_on_init_only: Option<bool>,
        pub ingress_host_key: Option<String>,
    }

    impl EnvVars {
       pub fn parse() -> Self {
            let envs = match envy::from_env::<Self>() {
                Ok(config) => config,
                Err(error) => {
                    error!("Can't find mandatory environment variable set");
                    panic!("{:?}", error);
                }
            };
            return envs;
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    struct HelmRepo {
        name: String,
        url: String
    }

    #[derive(Deserialize, Debug)]
    pub struct DefaultValues {
        pub default_overrides: Vec<String>,
        pub chart_repo_url: String,
        pub chart_version: String,
        pub chart_name: String,
    }

    impl DefaultValues {
        pub fn app_overrides(&self) -> Option<Vec<(String,String)>> {
            if self.default_overrides.is_empty() {
                None
            } else {
                let mut overrides: Vec<(String,String)> = Vec::new();
                for app_override in &self.default_overrides {
                    let split_override = app_override.split('=').collect::<Vec<_>>();
                    overrides.push((split_override.first().unwrap().to_string(), split_override.last().unwrap().to_string()));
                }
                Some(overrides)
            }
        }
        pub fn deployment_overrides(&self) -> Vec<(String,String)> {
            vec![
                (String::from("chart_repo_url"), self.chart_repo_url.clone()),
                (String::from("chart_version"), self.chart_version.clone()),
                (String::from("chart_name"), self.chart_name.clone()),
            ]
        }
        pub fn deployment_keys(&self) -> Vec<String> {
            let mut keys: Vec<String> = vec![
                String::from("chart_repo_url"),
                String::from("chart_version"),
                String::from("chart_name"),
                String::from("chart_repo_user"),
                String::from("chart_repo_pass"),
            ];
            keys.sort();
            keys
        }
    }

    #[derive(Debug, Clone, Default)]
    pub struct HelmDeploymentParameters {
        pub chart_repo_url: String,
        pub chart_repo_user: String,
        pub chart_repo_pass: String,
        pub chart_version: String,
        pub chart_name: String,
    }

    impl HelmDeploymentParameters {
        pub fn new(cli_opts: &CliOpts, envs: &EnvVars) -> Self {
            let mut deployment_parameters = Self::default();
            deployment_parameters.chart_repo_user = envs.helm_user.clone();
            deployment_parameters.chart_repo_pass = envs.helm_pass.clone();
            for deployment_override in cli_opts.deployment_config.clone().into_iter() {
                let split_override = deployment_override.split('=').collect::<Vec<_>>();
                match split_override.first().unwrap().as_ref() {
                    "chart_repo_url" => deployment_parameters.chart_repo_url = split_override.last().unwrap().to_string(),
                    "chart_version" => deployment_parameters.chart_version = split_override.last().unwrap().to_string(),
                    "chart_name" => deployment_parameters.chart_name = split_override.last().unwrap().to_string(),
                    "chart_repo_user" => deployment_parameters.chart_repo_user = split_override.last().unwrap().to_string(),
                    "chart_repo_pass" => deployment_parameters.chart_repo_pass = split_override.last().unwrap().to_string(),
                    _ => continue,
                }
            }
            return deployment_parameters;
        }
        pub fn new_env_only(default_values: &DefaultValues, envs: &EnvVars) -> Self {
            Self {
                chart_repo_url:  default_values.chart_repo_url.clone(),
                chart_repo_user: envs.helm_user.clone(),
                chart_repo_pass: envs.helm_pass.clone(),
                chart_version: default_values.chart_version.clone(),
                chart_name: default_values.chart_name.clone(),
            }
        }
    }
    pub struct HelmCmd {
        pub name: String,
        pub mode: HelmMode,
        pub release_name: String,
        pub namespace: String,
        pub deployment_parameters: HelmDeploymentParameters,
        pub overrides: Vec<String>,
    }

    impl HelmCmd {
       pub fn args(&self) -> Vec<String> {
           let mut args: Vec<String> = Vec::new();
           let chart_repo_name = url_to_name(&self.deployment_parameters.chart_repo_url);
           let chart_location = format!("{}/{}", chart_repo_name, self.deployment_parameters.chart_name);
           let chart_version = vec![String::from("--version"), self.deployment_parameters.chart_version.clone()];
           let namespace = vec![String::from("-n"), String::from(&self.namespace),];
           let common_args = vec![
               String::from("--debug"),
           ];
           match self.mode {
               HelmMode::Uninstall => {
                   args.extend(namespace);
                   args.push(String::from("uninstall"));
                   args.push(String::from(&self.release_name));
               },
               HelmMode::UpgradeInstall => {
                   args.extend(namespace);
                   args.push(String::from("upgrade"));
                   args.push(String::from("-i"));
                   args.push(String::from(&self.release_name));
                   args.push(chart_location);
                   args.extend(chart_version);
                   args.extend(self.set_flag_values());
                   args.extend(common_args);
               },
               HelmMode::Template => {
                   args.extend(namespace);
                   args.push(String::from("template"));
                   args.push(String::from(&self.release_name));
                   args.push(chart_location);
                   args.extend(chart_version);
                   args.extend(self.set_flag_values());
               },
               HelmMode::ShowValues => {
                   args.extend(namespace);
                   args.push(String::from("show"));
                   args.push(String::from("values"));
                   args.push(chart_location);
                   args.extend(chart_version);
               },
               HelmMode::RepoAdd => {
                   args.push(String::from("repo"));
                   args.push(String::from("add"));
                   args.push(String::from("--force-update"));
                   args.push(String::from(&chart_repo_name));
                   args.push(String::from(&self.deployment_parameters.chart_repo_url));
                   args.extend(vec![String::from("--username"), String::from(&self.deployment_parameters.chart_repo_user)]);
                   args.extend(vec![String::from("--password"), String::from(&self.deployment_parameters.chart_repo_pass)]);
               },
               HelmMode::RepoUpdate => {
                   args.push(String::from("repo"));
                   args.push(String::from("update"));
               },
               HelmMode::RepoList => {
                   args.push(String::from("repo"));
                   args.push(String::from("list"));
                   args.push(String::from("-o"));
                   args.push(String::from("json"));
               },
           }

           return args;
        }
        fn set_flag_values(&self) -> Vec<String> {
            let mut values = Vec::new();
            let mut set_values = Vec::new();
            values.extend(self.overrides.clone());
            for value in values.into_iter() {
                set_values.push(String::from("--set"));
                set_values.push(value);
            }
            return set_values;
        }
       pub fn run(&self) -> Result<ExitStatus, ExitStatus> {
           let helm = Command::new(&self.name)
               .args(&self.args())
               .output()
               .expect("Failed to run");
           info!("helm stdout:\n {}", String::from_utf8(helm.stdout).unwrap());
           info!("helm stderr:\n {}", String::from_utf8(helm.stderr).unwrap());
           if helm.status.success() {
               Ok(helm.status)
           } else {
               Err(helm.status)
           }
       }
       pub fn run_stdout(&self) -> Result<String, (ExitStatus, String)> {
           let helm = Command::new(&self.name)
               .args(&self.args())
               .output()
               .expect("Failed to run");
           if helm.status.success() {
               Ok(String::from_utf8(helm.stdout).unwrap())
           } else {
               let stdout = String::from_utf8(helm.stdout).unwrap();
               let stderr = String::from_utf8(helm.stderr).unwrap();
               info!("helm stdout:\n {}", stdout);
               info!("helm stderr:\n {}", stderr);
               Err((helm.status, stderr))
           }
        }
    }
    pub enum HelmMode {
        Uninstall,
        UpgradeInstall,
        Template,
        RepoAdd,
        RepoUpdate,
        RepoList,
        ShowValues,
    }
    #[derive(CustomResource, Debug, Serialize, Deserialize, Default, Clone, JsonSchema)]
    #[kube(group = "acid.zalan.do", version = "v1", kind = "Postgresql", namespaced)]
    pub struct PosgresqlSpec {
        numberOfInstances: i32
    }
    #[derive(CustomResource, Debug, Serialize, Deserialize, Default, Clone, JsonSchema)]
    #[kube(group = "kafka.strimzi.io", version = "v1beta2", kind = "Kafka", namespaced)]
    pub struct KafkaSpec {
        kafka: KafkaConfig,
        zookeeper: ZookeeperConfig
    }
    #[derive(Debug, Serialize, Deserialize, Default, Clone, JsonSchema)]
    struct KafkaConfig {
        replicas: i32
    }
    #[derive(Debug, Serialize, Deserialize, Default, Clone, JsonSchema)]
    struct ZookeeperConfig {
        replicas: i32
    }
    fn check_value(value: String) -> Result<String, String> {
        let re = Regex::new(r"^([^=]*)=(.*)$").unwrap();
        match re.captures(&value) {
            Some(_) => return Ok(value),
            None => return Err(format!("Override value {} is malformed", value)),
        }
    }
    pub fn overrides(cli_opts: &CliOpts, envs: &EnvVars) -> Option<Vec<String>> {
        let mut overrides_opts = Vec::new();
        overrides_opts.extend(&cli_opts.application_config);
        let ingress_override = match &envs.ingress_host_key {
            Some(key) => format!("{}={}", &key, domain_name(&cli_opts)),
            None => format!("ingress.hostname={}", domain_name(&cli_opts)),
        };
        overrides_opts.push(&ingress_override);
        if overrides_opts.is_empty() {
            None
        } else {
            Some(overrides_opts
                 .into_iter()
                 .map(|value| check_value(value.to_string()).unwrap())
                 .collect::<Vec<_>>()
                )
        }
    }
    fn domain_name(cli_opts: &CliOpts) -> String {
        match &cli_opts.name {
            Some(name) => {
                return format!("{}.{}", name, &cli_opts.base_domain);
            },
            None => panic!("No name argument provided")
        }
    }
    pub fn parse_to_k8s(yaml: String) -> Result<(Vec<Deployment>, Vec<StatefulSet>, Vec<Ingress>, Vec<OldIngress>, Vec<Postgresql>, Vec<Kafka>), serde_yaml::Error> {
        let docs = YamlLoader::load_from_str(&yaml).unwrap();
        let mut deployments: Vec<Deployment> = Vec::new();
        let mut statefulsets: Vec<StatefulSet> = Vec::new();
        let mut ingresses: Vec<Ingress> = Vec::new();
        let mut old_ingresses: Vec<OldIngress> = Vec::new();
        let mut postgresqls: Vec<Postgresql> = Vec::new();
        let mut kafkas: Vec<Kafka> = Vec::new();
        for doc in docs {
            match doc["kind"].as_str().unwrap() {
                "Deployment" => {
                    let mut deployment_str = String::new();
                    let mut emitter = YamlEmitter::new(&mut deployment_str);
                    emitter.dump(&doc).unwrap();
                    let deployment: Deployment = serde_yaml::from_str(&deployment_str)?;
                    deployments.push(deployment);
                },
                "StatefulSet" => {
                    let mut statefulset_str = String::new();
                    let mut emitter = YamlEmitter::new(&mut statefulset_str);
                    emitter.dump(&doc).unwrap();
                    let statefulset: StatefulSet = serde_yaml::from_str(&statefulset_str)?;
                    statefulsets.push(statefulset);
                },
                "Ingress" => {
                    let mut ingress_str = String::new();
                    let mut emitter = YamlEmitter::new(&mut ingress_str);
                    emitter.dump(&doc).unwrap();
                    if doc["apiVersion"].as_str().unwrap() == "networking.k8s.io/v1beta1" {
                        let ingress: OldIngress = serde_yaml::from_str(&ingress_str)?;
                        old_ingresses.push(ingress);
                    }else{
                        let ingress: Ingress = serde_yaml::from_str(&ingress_str)?;
                        ingresses.push(ingress);
                    }
                },
                "postgresql" => {
                    let mut postgresql_str = String::new();
                    let mut emitter = YamlEmitter::new(&mut postgresql_str);
                    emitter.dump(&doc).unwrap();
                    let postgresql: Postgresql = serde_yaml::from_str(&postgresql_str)?;
                    postgresqls.push(postgresql);
                },
                "Kafka" => {
                    let mut kafka_str = String::new();
                    let mut emitter = YamlEmitter::new(&mut kafka_str);
                    emitter.dump(&doc).unwrap();
                    let kafka: Kafka = serde_yaml::from_str(&kafka_str)?;
                    kafkas.push(kafka);
                },
                _ => continue,
            }
        }
        if deployments.is_empty() && statefulsets.is_empty() {
            panic!("Got no deployment or stateful sets. Nothing to check");
        }
        return Ok((deployments, statefulsets, ingresses, old_ingresses, postgresqls, kafkas));
    }

    #[derive(Debug)]
    pub struct NoReplicasError(String);

    impl fmt::Display for NoReplicasError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "Deployment {} doesn't have any replicas", self.0)
        }
    }
    impl Error for NoReplicasError {}

    #[derive(Debug)]
    pub struct UnavailableReplicasError(String);

    impl fmt::Display for UnavailableReplicasError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "Deployment {} has unavailable replicas", self.0)
        }
    }
    impl Error for UnavailableReplicasError {}

    #[derive(Debug)]
    pub enum KubeError {
        NoReplicasError(NoReplicasError),
        UnavailableReplicasError(UnavailableReplicasError),
        KubeApiError(kube::Error),
    }

    impl From<NoReplicasError> for KubeError {
        fn from(error: NoReplicasError) -> Self {
            KubeError::NoReplicasError(error)
        }
    }

    impl From<UnavailableReplicasError> for KubeError {
        fn from(error: UnavailableReplicasError) -> Self {
            KubeError::UnavailableReplicasError(error)
        }
    }

    impl From<kube::Error> for KubeError {
        fn from(error: kube::Error) -> Self {
            KubeError::KubeApiError(error)
        }
    }

    #[derive(Debug)]
    pub enum ImageError {
        EcrError(DescribeImagesError),
        DockerRegistryError(RegError),
        NoImageError()
    }

    impl fmt::Display for ImageError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                ImageError::EcrError(err) => write!(f, "ECR error: {}", err),
                ImageError::DockerRegistryError(err) => write!(f, "Docker registry error: {}", err),
                ImageError::NoImageError() => write!(f, "No Image Error")
            }
        }
    }

    impl Error for ImageError {}


    impl From<DescribeImagesError> for ImageError {
        fn from(error: DescribeImagesError) -> Self {
            ImageError::EcrError(error)
        }
    }

    impl From<RegError> for ImageError {
        fn from(error: RegError) -> Self {
            ImageError::DockerRegistryError(error)
        }
    }


    #[tokio::main]
    async fn check_deployment(namespace: &str, name: &str) -> Result<(), KubeError> {
        let client = Client::try_default().await?;
        let api: Api<Deployment> = Api::namespaced(client, &namespace);
        let deployment = api.get(&name).await?;
        info!("AAA {:?}", deployment.status);
        match deployment.status {
            Some(status) => {
                match (status.ready_replicas) {
                    Some(ready_replicas) => {
                        if Some(ready_replicas) == status.replicas {
                            return Ok(());
                        } else {
                            return Err(KubeError::NoReplicasError(NoReplicasError(name.to_string())));
                        }
                    },
                    None => {
                        return Err(KubeError::NoReplicasError(NoReplicasError(name.to_string())));
                    }
                }
            },
            None => {
                return Err(KubeError::NoReplicasError(NoReplicasError(name.to_string())));
            }
        }
        Ok(())
    }
    #[tokio::main]
    async fn check_statefulset(namespace: &str, name: &str) -> Result<(), KubeError> {
        let client = Client::try_default().await?;
        let api: Api<StatefulSet> = Api::namespaced(client, &namespace);
        let statefulset = api.get(&name).await?;
        match statefulset.status {
            Some(status) => {
                match (status.ready_replicas) {
                    Some(ready_replicas) => {
                        if ready_replicas == status.replicas {
                            return Ok(());
                        } else {
                            return Err(KubeError::NoReplicasError(NoReplicasError(name.to_string())));
                        }
                    },
                    None => {
                        return Err(KubeError::NoReplicasError(NoReplicasError(name.to_string())));
                    }
                }
            },
            None => {
                return Err(KubeError::NoReplicasError(NoReplicasError(name.to_string())));
            }
        }
        Ok(())
    }
    pub fn check_all(deployments: Vec<Deployment>, statefulsets: Vec<StatefulSet>, namespace: String) -> Result<(), KubeError> {
        if deployments.is_empty() {
            info!("No deployments to check");
        }else{
            for deploy in deployments {
                match check_deployment(&namespace, &deploy.name()) {
                    Ok(status) => status,
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
        }

        if statefulsets.is_empty() {
            info!("No statefulsets to check");
        }else{
            for statefulset in statefulsets {
                match check_statefulset(&namespace, &statefulset.name()) {
                    Ok(status) => status,
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
        }
        Ok(())
    }
    //TODO: We seriously need a generic here
    pub fn ingresses_to_hosts(new_ingresses: Vec<Ingress>, old_ingresses: Vec<OldIngress>, default_name: String) -> Option<Vec<(String, String)>> {
        let mut hosts: Vec<(String, String)> = Vec::new();
        if new_ingresses.is_empty() {
            for ingress in old_ingresses {
                let ingress_name = match &ingress.metadata.name {
                    Some(name) => name,
                    None => {
                        warn!("Can't parse ingress name");
                        &default_name
                    }
                };
                match &ingress.spec {
                    Some(spec) => {
                        let scheme = if spec.tls.is_empty() {
                            "http"
                        }else{
                            "https"
                        };
                        for rule in &spec.rules {
                            match &rule.host {
                                Some(host) => hosts.push((ingress_name.to_string(), format!("{}://{}",scheme, host))),
                                None => continue
                            }
                        }
                    },
                    None => continue
                }
            }
        }else{
            for ingress in new_ingresses {
                let ingress_name = match &ingress.metadata.name {
                    Some(name) => name,
                    None => {
                        warn!("Can't parse ingress name");
                        &default_name
                    }
                };
                match &ingress.spec {
                    Some(spec) => {
                        let scheme = if spec.tls.is_empty() {
                            "http"
                        }else{
                            "https"
                        };
                        for rule in &spec.rules {
                            match &rule.host {
                                Some(host) => hosts.push((ingress_name.to_string(), format!("{}://{}",scheme, host))),
                                None => continue
                            }
                        }
                    },
                    None => continue
                }
            }
        }
        if hosts.is_empty() {
            None
        }else{
            Some(hosts)
        }
    }
    pub fn print_kv(kv: Option<Vec<(String,String)>>) -> String {
        let mut out_string = String::from("");
        match kv {
            Some(values) => {
                for value in values {
                    out_string.push_str(&format!("{},{}\n", value.0, value.1));
                }
            },
            None => return out_string
        }
        return out_string;
    }
    pub fn print_keys(vals: Option<Vec<String>>) -> String {
        let mut out_string = String::from("");
        match vals {
            Some(values) => {
                for value in values {
                    out_string.push_str(&format!("{}\n", value));
                }
            },
            None => return out_string
        }
        return out_string;
    }
    //TODO: Generics!!!
    pub fn deployments_statefulsets_to_images(deployments: Vec<Deployment>, statefulsets: Vec<StatefulSet>) -> Option<Vec<String>> {
        let mut images: Vec<String> = Vec::new();
        if deployments.is_empty() {
            info!("No deployments to extract images from");
        }else{
            for deployment in deployments {
                let deployment_name = match &deployment.metadata.name {
                    Some(name) => name,
                    None => "NoName!"
                };
                match &deployment.spec {
                    Some(spec) => match &spec.template.spec {
                        Some(podspec) => {
                            for container in &podspec.containers {
                                match &container.image {
                                    Some(image) => images.push(String::from(image)),
                                    None => warn!("No Image found for container {}", container.name)
                                }
                            }
                        },
                        None => warn!("No podspec in deployment {}", &deployment_name),
                    },
                    None => warn!("No deployment spec in deployment {}", &deployment_name),
                }
            }
        }
        if statefulsets.is_empty() {
            info!("No statefulsets to extract images from");
        }else{
            for statefulset in statefulsets {
                let statefulset_name = match &statefulset.metadata.name {
                    Some(name) => name,
                    None => "NoName!"
                };
                match &statefulset.spec {
                    Some(spec) => match &spec.template.spec {
                        Some(podspec) => {
                            for container in &podspec.containers {
                                match &container.image {
                                    Some(image) => images.push(String::from(image)),
                                    None => warn!("No Image found for container {}", container.name)
                                }
                            }
                        },
                        None => warn!("No podspec in statefulset {}", &statefulset_name),
                    },
                    None => warn!("No statefulset spec in statefulset {}", &statefulset_name),
                }
            }
        }
        if images.is_empty() {
            None
        }else{
            Some(images)
        }
    }
    #[tokio::main]
    async fn check_docker_image(registry: &str, repository: &str, tag: &str) -> Result<(), ImageError> {
        let client = RegClient::configure()
            .insecure_registry(false)
            .registry(registry)
            .build()?;
        let aclient = client.authenticate(&[&format!("repository:{}:pull", repository)]).await?;
        match aclient.has_manifest(repository, tag, None).await? {
            Some(_) => return Ok(()),
            None =>  return Err(ImageError::NoImageError())//panic!("Image not found: {} {} {}", registry, repository, tag),
        }
    }
    #[tokio::main]
    pub async fn check_ecr_image(registry: &str, repository: &str, tag: &str) -> Result<(), ImageError>{
        let re = Regex::new(r"^[0-9]*.dkr.ecr.(.*).amazonaws.com$").unwrap();
        let captures = re.captures(&registry).unwrap();
        let region_string = String::from(&captures[1]);
        let region: Region = region_string.parse().unwrap();
        let client = EcrClient::new(region);
        let image_id = ImageIdentifier {
            image_tag: Some(String::from(tag)),
            image_digest: None,
        };
        let describe_request = DescribeImagesRequest {
            repository_name: String::from(repository),
            image_ids: Some(vec![image_id]),
            filter: None,
            max_results: None,
            next_token: None,
            registry_id: None,
        };
        match client.describe_images(describe_request).await {
            Ok(_) => return Ok(()),
            Err(err) => return Err(ImageError::NoImageError()) //panic!("Image not found in ECR: {} {} {}\n {}", registry, repository, tag, err),
        }
    }
    fn parse_image_name(image_name: &str) -> (String, String, String) {
        let mut registry = String::new();
        let mut repository = String::new();
        let re = Regex::new(r"^([^/:]*)/??([^/:]*)/??([^/:]*):??([^/:]*)$").unwrap();
        let captures = re.captures(&image_name).unwrap();
        let tag = String::from(&captures[4]);
        if &captures[2] == "" && &captures[3] == "" {
            registry = String::from("registry-1.docker.io");
            repository = format!("library/{}", &captures[1])
        } else if &captures[2] == "" {
            registry = String::from(&captures[1]);
            repository = String::from(&captures[3]);
        } else {
            if &captures[1] == "docker.io" {
                registry = String::from("registry-1.docker.io");
            }else{
                registry = String::from(&captures[1]);
            }
            repository = format!("{}/{}", &captures[2], &captures[3]);
        }
        info!("Checking image: {} {} {}", registry, repository, tag);
        (registry, repository, tag)
    }
    pub fn check_images(images: Vec<String>) -> Result<(), ImageError> {
        for image in images {
            let (registry, repository, tag) = parse_image_name(&image);
            if registry.contains("dkr.ecr") {
                match check_ecr_image(&registry, &repository, &tag) {
                    Ok(_) => (),
                    Err(err) => return Err(err),
                }
            } else {
                match check_docker_image(&registry, &repository, &tag) {
                    Ok(_) => (),
                    Err(err) => return Err(err),
                }
            }
        }
        Ok(())
    }
    pub fn helm_repo_update(envs: &EnvVars, deployment_parameters: &HelmDeploymentParameters) {
        let helm_repo_update_ = HelmCmd {
            name: String::from(&envs.helm_bin),
            mode: HelmMode::RepoUpdate,
            release_name: String::from(""),
            namespace: String::from(""),
            deployment_parameters: deployment_parameters.clone(),
            overrides: vec![],
        };
        match helm_repo_update_.run() {
            Ok(_status) => info!("Repo update success!"),
            Err(status) => {
                println!("Couldn't update repo.");
                error!("Error during helm execution");
                panic!("{:?}", status);
            }
        }
    }
    pub fn helm_repo_add_update(envs: &EnvVars, deployment_parameters: &HelmDeploymentParameters) {
        let helm_repo_add = HelmCmd {
            name: String::from(&envs.helm_bin),
            mode: HelmMode::RepoAdd,
            release_name: String::from(""),
            namespace: String::from(""),
            deployment_parameters: deployment_parameters.clone(),
            overrides: vec![],
        };
        let helm_repo_update = HelmCmd {
            name: String::from(&envs.helm_bin),
            mode: HelmMode::RepoUpdate,
            release_name: String::from(""),
            namespace: String::from(""),
            deployment_parameters: deployment_parameters.clone(),
            overrides: vec![],
        };
        match helm_repo_add.run() {
            Ok(_status) => info!("Repo add success!"),
            Err(status) => {
                println!("Couldn't add repo.");
                error!("Error during helm execution");
                panic!("{:?}", status);
            }
        }
        match helm_repo_update.run() {
            Ok(_status) => info!("Repo update success!"),
            Err(status) => {
                println!("Couldn't update repo.");
                error!("Error during helm execution");
                panic!("{:?}", status);
            }
        }
    }
    pub fn helm_init(envs: &EnvVars, deployment_parameters: &HelmDeploymentParameters) {
        match &envs.helm_on_init_only {
            Some(enabled) => {
                if *enabled {
                    info!("Skipping helm initialization, it must be initialied on init");
                } else {
                    if helm_repo_exists(&envs, &deployment_parameters) {
                        info!("Skipping helm initialization, because requested repo was already added, just update");
                        helm_repo_update(&envs, &deployment_parameters);
                    } else {
                        info!("Starting helm initialization");
                        helm_repo_add_update(&envs, &deployment_parameters);
                    }
                }
            },
            None => {
                if helm_repo_exists(&envs, &deployment_parameters) {
                    info!("Skipping helm initialization, because requested repo was already added, just update");
                    helm_repo_update(&envs, &deployment_parameters);
                } else {
                    info!("Starting helm initialization");
                    helm_repo_add_update(&envs, &deployment_parameters);
                }
            }
        }
    }

    fn helm_repo_exists(envs: &EnvVars, deployment_parameters: &HelmDeploymentParameters) -> bool {
        let chart_repo_url = String::from(&deployment_parameters.chart_repo_url);
        let chart_repo_name = url_to_name(&deployment_parameters.chart_repo_url);
        let helm_repo_list = HelmCmd {
            name: String::from(&envs.helm_bin),
            mode: HelmMode::RepoList,
            release_name: String::from(""),
            namespace: String::from(""),
            deployment_parameters: deployment_parameters.clone(),
            overrides: vec![],
        };
        match helm_repo_list.run_stdout() {
            Ok(list) => {
                let helm_repos: Vec<HelmRepo> = serde_json::from_str(&list).unwrap();
                for repo in helm_repos {
                    if repo.name == chart_repo_name && repo.url == chart_repo_url {
                       return true;
                    } else {
                        continue
                    }
                }
            },
            Err(_err) => {
                return false;
            }
        }
        false
    }
    pub fn helm_values_as_keys(yaml: String) -> Option<Vec<String>> {
        let mut keys: Vec<String> = Vec::new();
        let docs = YamlLoader::load_from_str(&yaml).unwrap();
        let docs_hash = &docs[0].clone().into_hash().unwrap();
        for doc in docs_hash {
            match doc.1.as_hash() {
                Some(hash) => {
                    if hash.is_empty() {
                        keys.push(format!("{}", doc.0.as_str().unwrap()));
                    }else{
                        for doc_2 in hash {
                            match doc_2.1.as_hash() {
                                Some(hash_2) => {
                                    if hash_2.is_empty() {
                                        keys.push(format!("{}.{}", doc.0.as_str().unwrap(), doc_2.0.as_str().unwrap()));
                                    }else{
                                        for key in hash_2.keys() {
                                            keys.push(format!("{}.{}.{}", doc.0.as_str().unwrap(), doc_2.0.as_str().unwrap(), key.as_str().unwrap()));
                                        }
                                    }
                                },
                                None => keys.push(format!("{}.{}", doc.0.as_str().unwrap(), doc_2.0.as_str().unwrap()))
                            }
                        }
                    }
                },
                None => keys.push(format!("{}", doc.0.as_str().unwrap())),
            }
        }
        if keys.is_empty() {
            None
        }else{
            keys.sort();
            Some(keys)
        }
    }
    #[tokio::main]
    async fn scale_deployment(namespace: &str, name: &str, replicas: i32) -> Result<(), kube::Error> {
        let client = Client::try_default().await?;
        let api: Api<Deployment> = Api::namespaced(client, &namespace);
        let deployment = api.get(&name).await?;
        let mut spec = match deployment.spec {
            Some(spec) => spec,
            None => panic!("Deployment without any spec!")
        };
        spec.replicas = Some(replicas);
        let patched_deployment = Deployment {
            metadata: deployment.metadata,
            spec: Some(spec),
            status: None
        };
        let patch = Patch::Merge(patched_deployment);
        let patch_params = PatchParams::apply("octopod");
        api.patch(&name, &patch_params, &patch).await?;
        Ok(())
    }
    #[tokio::main]
    async fn scale_statefulset(namespace: &str, name: &str, replicas: i32) -> Result<(), kube::Error> {
        let client = Client::try_default().await?;
        let api: Api<StatefulSet> = Api::namespaced(client, &namespace);
        let statefulset = api.get(&name).await?;
        let mut spec = match statefulset.spec {
            Some(spec) => spec,
            None => panic!("StatefulSet without any spec!")
        };
        spec.replicas = Some(replicas);
        let patched_statefulset = StatefulSet {
            metadata: statefulset.metadata,
            spec: Some(spec),
            status: None
        };
        let patch = Patch::Merge(patched_statefulset);
        let patch_params = PatchParams::apply("octopod");
        api.patch(&name, &patch_params, &patch).await?;
        Ok(())
    }
    #[tokio::main]
    async fn scale_postgresql(namespace: &str, name: &str, replicas: i32) -> Result<(), kube::Error> {
        let client = Client::try_default().await?;
        let api: Api<Postgresql> = Api::namespaced(client, &namespace);
        let postgres = api.get(&name).await?;
        println!("{:#?}", postgres);
        let mut spec = postgres.spec;
        spec.numberOfInstances = replicas;
        let patched_postgres = Postgresql {
            api_version: postgres.api_version,
            kind: postgres.kind,
            metadata: postgres.metadata,
            spec: spec,
        };
        let patch = Patch::Merge(patched_postgres);
        let patch_params = PatchParams::apply("octopod");
        api.patch(&name, &patch_params, &patch).await?;
        Ok(())
    }
    #[tokio::main]
    async fn scale_kafka(namespace: &str, name: &str, replicas: i32) -> Result<(), kube::Error> {
        let client = Client::try_default().await?;
        let api: Api<Kafka> = Api::namespaced(client.clone(), &namespace);
        let kafka = api.get(&name).await?;
        let metadata = kafka.metadata;
        if replicas > 0 {
            if metadata.annotations.contains_key("strimzi.io/pause-reconciliation") {
                println!("deleting pause-reconciliation annotation");
                let remove_op = json_patch::RemoveOperation {
                    path: String::from("/metadata/annotations/strimzi.io~1pause-reconciliation")
                };
                let patch_op = json_patch::PatchOperation::Remove(remove_op);
                let json_patch = json_patch::Patch(vec![patch_op]);
                let patch = Patch::Json::<()>(json_patch);
                let patch_params = PatchParams::apply("octopod");
                api.patch(&name, &patch_params, &patch).await?;
            }
        } else {
            if !metadata.annotations.contains_key("strimzi.io/pause-reconciliation") {
                println!("adding pause-reconciliation annotation");
                let add_op = json_patch::AddOperation {
                    path: String::from("/metadata/annotations/strimzi.io~1pause-reconciliation"),
                    value: serde_json::value::Value::String("true".to_string())
                };
                let patch_op = json_patch::PatchOperation::Add(add_op);
                let json_patch = json_patch::Patch(vec![patch_op]);
                let patch = Patch::Json::<()>(json_patch);
                let patch_params = PatchParams::apply("octopod");
                api.patch(&name, &patch_params, &patch).await?;
            }
        }

        let kafka_sts: Api<StatefulSet> = Api::namespaced(client, &namespace);
        let sts_list = ListParams::default().labels(&format!("strimzi.io/cluster={}", name));
        for sts in kafka_sts.list(&sts_list).await? {
            let sts_namespace = String::from(namespace);
            let sts_replicas = replicas.clone();
            task::spawn_blocking(move || {
                scale_statefulset(&sts_namespace, &sts.name(), sts_replicas);
            }).await.expect("Failed to run thread")
        }
        Ok(())
    }
    pub fn scale(deployments: Vec<Deployment>, statefulsets: Vec<StatefulSet>, postgresqls: Vec<Postgresql>, kafkas: Vec<Kafka>, namespace: String, replicas: i32) -> Result<(), Box<dyn error::Error>> {
        if deployments.is_empty() {
            info!("No deployments to check");
        }else{
            for deploy in deployments {
                match scale_deployment(&namespace, &deploy.name(), replicas) {
                    Ok(status) => status,
                    Err(err) => {
                        return Err(Box::new(err));
                    }
                }
            }
        }

        if statefulsets.is_empty() {
            info!("No statefulsets to check");
        }else{
            for statefulset in statefulsets {
                match scale_statefulset(&namespace, &statefulset.name(), replicas) {
                    Ok(status) => status,
                    Err(err) => {
                        return Err(Box::new(err));
                    }
                }
            }
        }

        if postgresqls.is_empty() {
            info!("No postgresqls to check");
        }else{
            for postgres in postgresqls {
                match scale_postgresql(&namespace, &postgres.name(), replicas) {
                    Ok(status) => status,
                    Err(err) => {
                        return Err(Box::new(err));
                    }
                }
            }
        }

        if kafkas.is_empty() {
            info!("No kafkas to check");
        }else{
            for kafka in kafkas {
                match scale_kafka(&namespace, &kafka.name(), replicas) {
                    Ok(status) => status,
                    Err(err) => {
                        return Err(Box::new(err));
                    }
                }
            }
        }
        Ok(())
    }
    pub fn ingresses_to_secrets(new_ingresses: Vec<Ingress>, old_ingresses: Vec<OldIngress>) -> Option<Vec<String>> {
        let mut secrets: Vec<String> = Vec::new();
        if new_ingresses.is_empty() {
            for ingress in old_ingresses {
                match &ingress.spec {
                    Some(spec) => {
                        if spec.tls.is_empty() {
                            continue
                        }else{
                            for tls in &spec.tls {
                                match &tls.secret_name {
                                    Some(secret) => secrets.push(String::from(secret)),
                                    None => continue
                                }
                            }
                        }
                    },
                    None => continue
                }
            }
        }else{
            for ingress in new_ingresses {
                match &ingress.spec {
                    Some(spec) => {
                        if spec.tls.is_empty() {
                            continue
                        }else{
                            for tls in &spec.tls {
                                match &tls.secret_name {
                                    Some(secret) => secrets.push(String::from(secret)),
                                    None => continue
                                }
                            }
                        }
                    },
                    None => continue
                }
            }
        }
        if secrets.is_empty() {
            None
        }else{
            Some(secrets)
        }
    }
    #[tokio::main]
    pub async fn delete_pvcs(namespace: &str, selector: &str) -> Result<(), kube::Error> {
        let client = Client::try_default().await?;
        let api: Api<PersistentVolumeClaim> = Api::namespaced(client, &namespace);
        let pvc_list = ListParams::default().labels(selector);
        api.delete_collection(&DeleteParams::default(), &pvc_list).await?;
        Ok(())
    }
    #[tokio::main]
    pub async fn delete_secret(namespace: &str, secret: &str) -> Result<(), kube::Error> {
        let client = Client::try_default().await?;
        let api: Api<Secret> = Api::namespaced(client, &namespace);
        api.delete(&secret, &DeleteParams::default()).await?;
        Ok(())
    }
    fn url_to_name(url: &str) -> String {
        let re_not_letters = Regex::new(r"\W").unwrap();
        let name: String = re_not_letters.replace_all(url, "-").to_string();
        return name;
    }
}
