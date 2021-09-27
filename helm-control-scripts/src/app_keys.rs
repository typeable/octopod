use helm_control_scripts::lib::*;

fn main() {
    let mut log_builder = Builder::from_default_env();
    log_builder.target(Target::Stderr).filter(None, LevelFilter::Info).init();
    info!("Utils version {}", env!("CARGO_PKG_VERSION"));
    let envs = EnvVars::parse();
    info!("Env variables received {:?}", &envs);
    let cli_opts = CliOpts::from_args();
    info!("Cli options received {:?}", &cli_opts);
    let deployment_parameters = HelmDeploymentParameters::new(&cli_opts, &envs);
    helm_init(&envs, &deployment_parameters);
    let helm_values = HelmCmd {
        name: envs.helm_bin,
        mode: HelmMode::ShowValues,
        release_name: String::from(""),
        release_domain: String::from(""),
        namespace: String::from(""),
        deployment_parameters: deployment_parameters,
        overrides: vec![],
    };
    info!("Generated Helm args: {:?}", &helm_values.args());
    match helm_values.run_stdout() {
        Ok(status) => {
          print!("{}", print_keys(helm_values_as_keys(status)));
        }
        Err(status) => {
            error!("Error during helm execution");
            panic!("{:?}", status);
        }
    }
}
