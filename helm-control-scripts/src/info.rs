use helm_control_scripts::lib::*;

fn main() {
    let mut log_builder = Builder::from_default_env();
    log_builder.target(Target::Stderr).filter(None, LevelFilter::Info).init();
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
    helm_init(&envs, &deployment_parameters);
    let release_name = match cli_opts.name {
        Some(name) => name,
        None => {
            error!("mandatory name argument was not provided");
            panic!();
        }
    };
    let default_name = String::from(&release_name);
    let helm_template = HelmCmd {
        name: envs.helm_bin,
        mode: HelmMode::Template,
        release_name: release_name,
        namespace: cli_opts.namespace,
        deployment_parameters: deployment_parameters,
        overrides: overrides,
    };
    match helm_template.run_stdout() {
        Ok(status) => {
            let (_deployments, _statefulsets, ingresses, old_ingresses) = match parse_to_k8s(status) {
                Ok((deployments, statefulsets, ingresses, old_ingresses)) => (deployments, statefulsets, ingresses, old_ingresses),
                Err(err) => panic!("{}", err)
            };
            print!("{}", print_kv(ingresses_to_hosts(ingresses, old_ingresses, default_name)));
        }
        Err(status) => {
            error!("Error during helm execution");
            panic!("{:?}", status);
        }
    }
}
