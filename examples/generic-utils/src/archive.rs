use generic_utils::lib::*;

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
    let helm_cmd = HelmCmd {
        name: envs.helm_bin,
        mode: HelmMode::Uninstall,
        release_name: cli_opts.name,
        release_domain: String::from(""),
        namespace: cli_opts.namespace,
        deployment_parameters: deployment_parameters,
        overrides: vec![],
        default_values: vec![],
        image_tag: String::from("")
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
