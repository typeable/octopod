use helm_control_scripts::lib::*;

fn main() {
    let mut log_builder = Builder::from_default_env();
    log_builder.target(Target::Stdout).filter(None, LevelFilter::Info).init();
    info!("Utils version {}", env!("CARGO_PKG_VERSION"));
    let envs = EnvVars::parse();
    info!("Env variables received {:?}", &envs);
    let default_values: DefaultValues = serde_json::from_str(&envs.defaults).unwrap();
    let deployment_parameters = HelmDeploymentParameters::new_env_only(&default_values, &envs);
    match &envs.helm_on_init_only {
        Some(enabled) => {
            if *enabled {
                helm_repo_add_update(&envs, &deployment_parameters);
            } else {
                info!("Skipping helm initialization since HELM_ON_INIT_ONLY is false");
            }
        },
        None => info!("Skipping helm initialization since HELM_ON_INIT_ONLY is not set"),
    }
    info!("Success!");
}
