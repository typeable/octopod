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

    let domain_name = domain_name(&cli_opts);
    info!("Domain generated for deployment: {}", &domain_name);
    let deployment_parameters = HelmDeploymentParameters::new(&cli_opts, &default_values, &envs);
    let image_tag = match cli_opts.tag {
        Some(tag) => tag,
        None => {
            error!("mandatory tag argument was not provided");
            panic!();
        }
    };
    helm_init(&envs, &deployment_parameters);
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
    match helm_template.run_stdout() {
        Ok(status) => {
            let (deployments, statefulsets, _ingresses, _old_ingresses) = match parse_to_k8s(status) {
                Ok((deployments, statefulsets, ingresses, old_ingresses)) => (deployments, statefulsets, ingresses, old_ingresses),
                Err(err) => panic!("{}", err)
            };
            match deployments_statefulsets_to_images(deployments, statefulsets) {
                Some(images) => {
                    match check_images(images) {
                        Ok(_) => info!("Success!"),
                        Err(err) => panic!("{}", err),
                    }
                },
                None => panic!("No images found!"),
            }
        }
        Err(status) => {
            error!("Error during helm execution");
            panic!("{:?}", status);
        }
    }
}
