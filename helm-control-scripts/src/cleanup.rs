use helm_control_scripts::lib::*;

fn main() {
    let mut log_builder = Builder::from_default_env();
    log_builder.target(Target::Stdout).filter(None, LevelFilter::Info).init();
    info!("Utils version {}", env!("CARGO_PKG_VERSION"));
    let envs = EnvVars::parse();
    info!("Env variables received {:?}", &envs);
    let default_values: DefaultValues = serde_json::from_str(&envs.defaults).unwrap();
    let cli_opts = CliOpts::from_args();
    info!("Cli options received {:?}", &cli_opts);
    let overrides = match overrides(&cli_opts) {
        Some(inner) => inner,
        None => vec![],
    };
    let deployment_parameters = HelmDeploymentParameters::new(&cli_opts, &default_values, &envs);
    let namespace = String::from(&cli_opts.namespace);
    let release_name = String::from(&cli_opts.name);
    let domain_name = domain_name(&cli_opts);
    let image_tag = match cli_opts.tag {
        Some(tag) => tag,
        None => {
            error!("mandatory tag argument was not provided");
            panic!();
        }
    };
    helm_init(&envs, &deployment_parameters);
    let helm_cmd = HelmCmd {
        name: envs.helm_bin.clone(),
        mode: HelmMode::Uninstall,
        release_name: cli_opts.name.clone(),
        release_domain: String::from(""),
        namespace: cli_opts.namespace.clone(),
        deployment_parameters: deployment_parameters.clone(),
        overrides: vec![],
        default_values: vec![],
        image_tag: String::from("")
    };
    let helm_template = HelmCmd {
        name: envs.helm_bin,
        mode: HelmMode::Template,
        release_name: cli_opts.name,
        release_domain: domain_name, 
        namespace: cli_opts.namespace,
        deployment_parameters: deployment_parameters,
        overrides: overrides,
        default_values: default_values.default_overrides,
        image_tag: image_tag
    };
    info!("Generated Helm args: {:?}", &helm_cmd.args());
    match helm_template.run_stdout() {
        Ok(status) => {
            let (_deployments, _statefulsets, ingresses, old_ingresses) = match parse_to_k8s(status) {
                Ok((deployments, statefulsets, ingresses, old_ingresses)) => (deployments, statefulsets, ingresses, old_ingresses),
                Err(err) => panic!("{}", err)
            };
            match helm_cmd.run() {
                Ok(_status) => {
                    match delete_pvcs(&namespace, &format!("app.kubernetes.io/instance={}", release_name)) {
                        Ok(_status) => info!("All pvcs were deleted successfully"),
                        Err(err) => {
                            error!("Error deleting pvcs");
                            panic!("{:?}", err);
                        }
                    }
                    match ingresses_to_secrets(ingresses, old_ingresses) {
                        Some(secrets) => {
                            for secret in secrets {
                                match delete_secret(&namespace, &secret) {
                                    Ok(_status) => info!("Successfully deleted secret {}", &secret),
                                    Err(error) => error!("Can't delete secret {}\n {}", &secret, error)
                                }
                            }
                        },
                        None => info!("No secrets to delete")
                    }
                    info!("Success!");
                },
                Err(status) => {
                    error!("Error during helm uninstall");
                    panic!("{:?}", status);
                }
            }            
        }
        Err(status) => {
            error!("Error during helm templating");
            panic!("{:?}", status);
        }
    }
}
