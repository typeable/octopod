use helm_control_scripts::lib::*;

fn main() {
    let mut log_builder = Builder::from_default_env();
    log_builder.target(Target::Stdout).filter(None, LevelFilter::Info).init();
    info!("Utils version {}", env!("CARGO_PKG_VERSION"));
    let envs = EnvVars::parse();
    info!("Env variables received {:?}", &envs);
    let cli_opts = CliOpts::from_args();
    info!("Cli options received {:?}", &cli_opts);
    let overrides = match overrides(&cli_opts, &envs) {
        Some(inner) => inner,
        None => vec![],
    };

    let deployment_parameters = HelmDeploymentParameters::new(&cli_opts, &envs);
    let namespace = String::from(&cli_opts.namespace);
    let release_name = match cli_opts.name {
        Some(name) => name,
        None => {
            error!("mandatory name argument was not provided");
            panic!();
        }
    };
    let helm_template = HelmCmd {
        name: envs.helm_bin,
        mode: HelmMode::Template,
        release_name: release_name,
        namespace: cli_opts.namespace,
        deployment_parameters: deployment_parameters,
        overrides: overrides,
    };
    info!("Generated Helm args: {:?}", &helm_template.args());
    match helm_template.run_stdout() {
        Ok(status) => {
            let (deployments, statefulsets, _ingresses, _old_ingresses, _postgresqls, _kafkas) = match parse_to_k8s(status) {
                Ok((deployments, statefulsets, ingresses, old_ingresses, postgresqls, kafkas)) => (deployments, statefulsets, ingresses, old_ingresses, postgresqls, kafkas),
                Err(err) => panic!("{}", err)
            };
            match check_all(deployments, statefulsets, namespace) {
                Ok(status) => {
                    error!("Deployment hasn't scaled down correctly");
                    panic!("{:?}", status);
                },
                Err(_status) => info!("Success!")
            }
        }
        Err(status) => {
            error!("Error during helm execution");
            panic!("{:?}", status);
        }
    }
}
