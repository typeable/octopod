use helm_control_scripts::lib::*;

fn main() {
    let mut log_builder = Builder::from_default_env();
    log_builder.target(Target::Stderr).filter(None, LevelFilter::Info).init();
    info!("Utils version {}", env!("CARGO_PKG_VERSION"));
    let envs = EnvVars::parse();
    info!("Env variables received {:?}", &envs);
    let default_values: DefaultValues = serde_json::from_str(&envs.defaults).unwrap();
    // let cli_opts = CliOpts::from_args();
    // info!("Cli options received {:?}", &cli_opts);
    // let domain_name = domain_name(&cli_opts);
    let deployment_parameters = HelmDeploymentParameters::new_env_only(&default_values, &envs);
    helm_init(&envs, &deployment_parameters);
    let helm_values = HelmCmd {
        name: envs.helm_bin,
        mode: HelmMode::ShowValues,
        release_name: String::from(""),
        release_domain: String::from(""),
        namespace: String::from(""),
        deployment_parameters: deployment_parameters,
        overrides: vec![],
        default_values: default_values.default_overrides,
        image_tag: String::from("")
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
