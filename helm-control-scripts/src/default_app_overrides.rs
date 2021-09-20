use helm_control_scripts::lib::*;

fn main() {
    let mut log_builder = Builder::from_default_env();
    log_builder.target(Target::Stderr).filter(None, LevelFilter::Info).init();
    info!("Utils version {}", env!("CARGO_PKG_VERSION"));
    let envs = EnvVars::parse();
    info!("Env variables received {:?}", &envs);
    let default_values: DefaultValues = serde_json::from_str(&envs.defaults).unwrap();
    let cli_opts = CliOpts::from_args();
    info!("Cli options received {:?}", &cli_opts);
    print!("{}", print_kv(default_values.app_overrides()));
}
