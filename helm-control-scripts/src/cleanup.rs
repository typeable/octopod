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
    let deployment_parameters = HelmDeploymentParameters::new(&cli_opts, &default_values, &envs);
    let kubectl_cmd =  KubectlCmd {
        name: envs.kubectl_bin,
        release_name: cli_opts.name,
        namespace: cli_opts.namespace,
        deployment_parameters: deployment_parameters,
    };
    kubectl_cmd.run();
    info!("Success!");
}
