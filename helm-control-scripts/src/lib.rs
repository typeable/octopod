pub mod lib {
    pub use std::io::prelude::*;
    pub use std::process::{Command, ExitStatus, Stdio};
    pub use structopt::StructOpt;
    pub use env_logger::{Builder, Target};
    pub use log::{LevelFilter, info, warn, error};
    pub use serde::{Serialize, Deserialize};
    pub use std::error;
    pub use yaml_rust::{YamlLoader, YamlEmitter};
    pub use k8s_openapi::api::{
        apps::v1::{Deployment, StatefulSet},
        networking::v1::Ingress,
        networking::v1beta1::Ingress as OldIngress
    };
    pub use kube::{
        api::{Api, ListParams, ResourceExt},
        Client,
    };
    pub use dkregistry::{
        v2::Client as RegClient,
        errors::Error
    };
    pub use regex::Regex;
    
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
        pub name: String,
        #[structopt(long)]
        pub tag: Option<String>,
        #[structopt(long)]
        pub app_env_override: Vec<String>,
        #[structopt(long)]
        pub deployment_override: Vec<String>,
    }
    
    #[derive(Deserialize, Debug)]
    pub struct EnvVars {
        pub helm_bin: String,
        pub kubectl_bin: String,
        pub defaults: String, //json of DefaultValues
        #[serde(default)]
        pub helm_user: String,
        #[serde(default)]
        pub helm_pass: String,
        pub helm_on_init_only: Option<bool>,
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
    
    #[derive(Deserialize, Debug)]
    pub struct DefaultValues {
        pub default_overrides: Vec<String>,
        pub chart_repo_url: String,
        pub chart_repo_name: String,
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
                (String::from("chart_repo_name"), self.chart_repo_name.clone()),
                (String::from("chart_version"), self.chart_version.clone()),
                (String::from("chart_name"), self.chart_name.clone()),
            ]   
        }
        pub fn deployment_keys(&self) -> Vec<String> {
            vec![
                String::from("chart_repo_url"),
                String::from("chart_repo_name"),
                String::from("chart_version"),
                String::from("chart_name"),
            ]   
        }
    }
        
    #[derive(Debug, Clone)]
    pub struct HelmDeploymentParameters {
        pub chart_repo_url: String,
        pub chart_repo_name: String,
        pub chart_repo_user: String,
        pub chart_repo_pass: String,
        pub chart_version: String,
        pub chart_name: String,
    }
    
    impl HelmDeploymentParameters {
        pub fn new(cli_opts: &CliOpts, default_values: &DefaultValues, envs: &EnvVars) -> Self {
            let mut default_parameters = Self {
                chart_repo_url:  default_values.chart_repo_url.clone(),
                chart_repo_name: default_values.chart_repo_name.clone(),
                chart_repo_user: envs.helm_user.clone(),
                chart_repo_pass: envs.helm_pass.clone(),
                chart_version: default_values.chart_version.clone(),
                chart_name: default_values.chart_name.clone(),
            };
            if cli_opts.deployment_override.is_empty() {
                return default_parameters;
            } else {
                for deployment_override in cli_opts.deployment_override.clone().into_iter() {
                    let split_override = deployment_override.split('=').collect::<Vec<_>>();
                    match split_override.first().unwrap().as_ref() {
                        "chart_repo_url" => default_parameters.chart_repo_url = split_override.last().unwrap().to_string(),
                        "chart_repo_name" => default_parameters.chart_repo_name = split_override.last().unwrap().to_string(),
                        "chart_version" => default_parameters.chart_version = split_override.last().unwrap().to_string(),
                        "chart_name" => default_parameters.chart_name = split_override.last().unwrap().to_string(),
                        _ => continue,
                    }
                }
                return default_parameters;
            }
        }
        pub fn new_env_only(default_values: &DefaultValues, envs: &EnvVars) -> Self {
            Self {
                chart_repo_url:  default_values.chart_repo_url.clone(),
                chart_repo_name: default_values.chart_repo_name.clone(),
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
        pub release_domain: String,
        pub namespace: String,
        pub deployment_parameters: HelmDeploymentParameters,
        pub overrides: Vec<String>,
        pub default_values: Vec<String>,
        pub image_tag: String,
    }
    
    impl HelmCmd {
       pub fn args(&self) -> Vec<String> {
            let mut args: Vec<String> = Vec::new();
            let chart_location = format!("{}/{}", self.deployment_parameters.chart_repo_name, self.deployment_parameters.chart_name);
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
                    args.push(String::from(&self.deployment_parameters.chart_repo_name));
                    args.push(String::from(&self.deployment_parameters.chart_repo_url));
                    args.extend(vec![String::from("--username"), String::from(&self.deployment_parameters.chart_repo_user)]);
                    args.extend(vec![String::from("--password"), String::from(&self.deployment_parameters.chart_repo_pass)]);
                },
                HelmMode::RepoUpdate => {
                    args.push(String::from("repo"));
                    args.push(String::from("update"));
                },
                HelmMode::Status => {
                    args.extend(namespace);
                    args.push(String::from("status"));
                    args.push(String::from(&self.release_name));
                },
                _ => panic!("This helm mode is not expected"),
            }
    
            return args;
        }
        fn set_flag_values(&self) -> Vec<String> {
            let mut values = Vec::new();
            let mut set_values = Vec::new();
            values.push(format!("image.tag={}", &self.image_tag));
            values.push(format!("ingress.host={}", &self.release_domain));
            values.extend(self.default_values.clone());
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
       pub fn run_stdout(&self) -> Result<String, ExitStatus> {
           let helm = Command::new(&self.name)
               .args(&self.args())
               .output()
               .expect("Failed to run");
           if helm.status.success() {
               Ok(String::from_utf8(helm.stdout).unwrap())
           } else {
               info!("helm stdout:\n {}", String::from_utf8(helm.stdout).unwrap());
               info!("helm stderr:\n {}", String::from_utf8(helm.stderr).unwrap());
               Err(helm.status)
           }
        }
    }
    pub enum HelmMode {
        Uninstall,
        UpgradeInstall,
        Template,
        RepoAdd,
        RepoUpdate,
        Status,
        ShowValues,
    }
    
    pub struct KubectlCmd {
        pub name: String,
        pub release_name: String,
        pub namespace: String,
        pub deployment_parameters: HelmDeploymentParameters,
    }

    impl KubectlCmd {
        pub fn args(&self, cmd_type: &str) -> Vec<String> {
            let mut args: Vec<String> = Vec::new();
            let namespace = vec![String::from("-n"), String::from(&self.namespace),];
            let cert_secret_name = format!("{}-{}-tls", &self.release_name, &self.deployment_parameters.chart_name);
            let label = vec![String::from("-l"), format!("app.kubernetes.io/instance={}", &self.release_name)];
            args.extend(namespace);
            args.push(String::from("delete"));
            match cmd_type {
                "pvc" => {
                    args.push(String::from("pvc"));
                    args.extend(label);
                    return args;
                },
                "cert" => {
                    args.push(String::from("secret"));
                    args.push(cert_secret_name);
                    return args;
                },
                _ => unreachable!(),
            }
        }
        pub fn run(&self) {
            info!("Starting pvc cleanup");
            info!("kubectl args: {:?}", &self.args("pvc"));
            let kubectl_pvc = Command::new(&self.name)
                .args(&self.args("pvc"))
                .output()
                .expect("Failed to run");
            info!("kubectl stdout:\n {}", String::from_utf8(kubectl_pvc.stdout).unwrap());
            info!("kubectl stderr:\n {}", String::from_utf8(kubectl_pvc.stderr).unwrap());
            info!("Starting certificates cleanup");
            info!("kubectl args: {:?}", &self.args("pvc"));
            let kubectl_cert = Command::new(&self.name)
                .args(&self.args("cert"))
                .output()
                .expect("Failed to run");
            info!("kubectl stdout:\n {}", String::from_utf8(kubectl_cert.stdout).unwrap());
            info!("kubectl stderr:\n {}", String::from_utf8(kubectl_cert.stderr).unwrap());
            assert!(kubectl_pvc.status.success());
            assert!(kubectl_cert.status.success());
        }
    }
    
    fn check_value(value: String) -> Result<String, String> {
        let split_value = value.split('=').collect::<Vec<_>>();
        if split_value.len() == 2 {
            Ok(value)
        } else {
            Err(format!("Override value {} is malformed", value))
        }
    }
    pub fn overrides(cli_opts: &CliOpts) -> Option<Vec<String>> {
        let mut overrides_opts = Vec::new();
        overrides_opts.extend(&cli_opts.app_env_override);
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
    pub fn domain_name(cli_opts: &CliOpts) -> String {
        format!("{}.{}", &cli_opts.name, &cli_opts.base_domain)
    }
    pub fn parse_to_k8s(yaml: String) -> Result<(Vec<Deployment>, Vec<StatefulSet>, Vec<Ingress>, Vec<OldIngress>), serde_yaml::Error> {
        let docs = YamlLoader::load_from_str(&yaml).unwrap();
        let mut deployments: Vec<Deployment> = Vec::new();
        let mut statefulsets: Vec<StatefulSet> = Vec::new();
        let mut ingresses: Vec<Ingress> = Vec::new();
        let mut old_ingresses: Vec<OldIngress> = Vec::new();
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
                _ => continue,
            }
        }
        if deployments.is_empty() && statefulsets.is_empty() {
            panic!("Got no deployment or stateful sets. Nothing to check");
        }
        return Ok((deployments, statefulsets, ingresses, old_ingresses));
    }
    #[tokio::main]
    async fn check_deployment(namespace: &str, name: &str) -> Result<(), kube::Error> {
        let client = Client::try_default().await?;
        let api: Api<Deployment> = Api::namespaced(client, &namespace);
        let deployment = api.get(&name).await?;
        match deployment.status {
            Some(status) => {
                match status.available_replicas {
                    Some(replicas) => {
                        if !replicas >= 1 {
                            panic!("Deployment {} doesn't have any replicas", &name);
                        }
                    },
                    None => {
                        panic!("Deployment {} doesn't have any replicas", &name);
                    }
                }
            },
            None => {
                panic!("Unable to get status for deployment {}", &name);
            }
        }
        Ok(())
    }
    #[tokio::main]
    async fn check_statefulset(namespace: &str, name: &str) -> Result<(), kube::Error> {
        let client = Client::try_default().await?;
        let api: Api<StatefulSet> = Api::namespaced(client, &namespace);
        let statefulset = api.get(&name).await?;
        match statefulset.status {
            Some(status) => {
                match status.current_replicas {
                    Some(replicas) => {
                        if !replicas >= 1 {
                            panic!("StatefulSet {} doesn't have any replicas", &name);
                        }
                    },
                    None => {
                        panic!("StatefulSet {} doesn't have any replicas", &name);
                    }
                }
            },
            None => {
                panic!("Unable to get status for StatefulSet {}", &name);
            }
        }
        Ok(())
    }
    pub fn check_all(deployments: Vec<Deployment>, statefulsets: Vec<StatefulSet>, namespace: String) -> Result<(), Box<dyn error::Error>> {
        if deployments.is_empty() {
            info!("No deployments to check");
        }else{
            for deploy in deployments {
                match check_deployment(&namespace, &deploy.name()) {
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
                match check_statefulset(&namespace, &statefulset.name()) {
                    Ok(status) => status,
                    Err(err) => {
                        return Err(Box::new(err));
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
    async fn check_image(registry: &str, repository: &str, tag: &str) -> Result<(), dkregistry::errors::Error> {
        let client = RegClient::configure()
            .insecure_registry(false)
            .registry(registry)
            .build()?;
        let aclient = client.authenticate(&[&format!("repository:{}:pull", repository)]).await?;
        match aclient.has_manifest(repository, tag, None).await? {
            Some(_) => return Ok(()),
            None => panic!("Image not found: {} {} {}", registry, repository, tag),
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
            registry = String::from("registry-1.docker.io");
            repository = format!("{}/{}", &captures[1], &captures[3]);
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
    pub fn check_images(images: Vec<String>) -> Result<(), dkregistry::errors::Error> {
        for image in images {
            let (registry, repository, tag) = parse_image_name(&image);
            match check_image(&registry, &repository, &tag) {
                Ok(_) => (),
                Err(err) => return Err(err),
            }
        }
        Ok(())
    }
    pub fn helm_repo_add_update(envs: &EnvVars, deployment_parameters: &HelmDeploymentParameters) {
        let helm_repo_add = HelmCmd {
            name: String::from(&envs.helm_bin),
            mode: HelmMode::RepoAdd,
            release_name: String::from(""),
            release_domain: String::from(""),
            namespace: String::from(""),
            deployment_parameters: deployment_parameters.clone(),
            overrides: vec![],
            default_values: vec![],
            image_tag: String::from(""),
        };
        let helm_repo_update = HelmCmd {
            name: String::from(&envs.helm_bin),
            mode: HelmMode::RepoUpdate,
            release_name: String::from(""),
            release_domain: String::from(""),
            namespace: String::from(""),
            deployment_parameters: deployment_parameters.clone(),
            overrides: vec![],
            default_values: vec![],
            image_tag: String::from(""),
        };
        match helm_repo_add.run() {
            Ok(_status) => info!("Repo add success!"),
            Err(status) => {
                error!("Error during helm execution");
                panic!("{:?}", status);
            }
        }
        match helm_repo_update.run() {
            Ok(_status) => info!("Repo update success!"),
            Err(status) => {
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
                    info!("Starting helm initialization");
                    helm_repo_add_update(&envs, &deployment_parameters);
                }
            },
            None => {
                info!("Starting helm initialization");
                helm_repo_add_update(&envs, &deployment_parameters);
            }
        }
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
                None => continue
            }
        }
        if keys.is_empty() {
            None
        }else{
            Some(keys)
        }
    }
}
