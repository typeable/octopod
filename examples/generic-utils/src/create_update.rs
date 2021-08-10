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
    let helm_repo_add = HelmCmd {
        name: String::from(&envs.helm_bin),
        mode: HelmMode::RepoAdd,
        release_name: String::from(""),
        release_domain: String::from(""),
        namespace: String::from(""),
        deployment_parameters: deployment_parameters.clone(),
        overrides: vec![],
        default_values: vec![],
        image_tag: String::from(""),
    };
    let helm_repo_update = HelmCmd {
        name: String::from(&envs.helm_bin),
        mode: HelmMode::RepoUpdate,
        release_name: String::from(""),
        release_domain: String::from(""),
        namespace: String::from(""),
        deployment_parameters: deployment_parameters.clone(),
        overrides: vec![],
        default_values: vec![],
        image_tag: String::from(""),
    };
    let helm_cmd = HelmCmd {
        name: envs.helm_bin,
        mode: HelmMode::UpgradeInstall,
        release_name: cli_opts.name,
        release_domain: domain_name, 
        namespace: cli_opts.namespace,
        deployment_parameters: deployment_parameters,
        overrides: overrides,
        default_values: default_values.default_overrides,
        image_tag: image_tag,
    };
    info!("Generated Helm args: {:?}", &helm_cmd.args());
    match helm_repo_add.run() {
        Ok(_status) => info!("Repo add success!"),
        Err(status) => {
            error!("Error during helm execution");
            panic!("{:?}", status);
        }
    }
    match helm_repo_update.run() {
        Ok(_status) => info!("Repo update success!"),
        Err(status) => {
            error!("Error during helm execution");
            panic!("{:?}", status);
        }
    }
    match helm_cmd.run() {
        Ok(_status) => info!("Success!"),
        Err(status) => {
            error!("Error during helm execution");
            panic!("{:?}", status);
        }
    }
}
