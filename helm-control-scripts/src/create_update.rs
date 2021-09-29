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
    helm_init(&envs, &deployment_parameters);
    let release_name = match cli_opts.name {
        Some(name) => name,
        None => {
            error!("mandatory name argument was not provided");
            panic!();
        }
    };
    let helm_cmd = HelmCmd {
        name: envs.helm_bin,
        mode: HelmMode::UpgradeInstall,
        release_name: release_name,
        namespace: cli_opts.namespace,
        deployment_parameters: deployment_parameters,
        overrides: overrides,
    };
    info!("Generated Helm args: {:?}", &helm_cmd.args());
    match helm_cmd.run() {
        Ok(_status) => info!("Success!"),
        Err(status) => {
            error!("Error during helm execution");
            panic!("{:?}", status);
        }
    }
}
