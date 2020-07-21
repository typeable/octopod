use fs_extra::dir;
use rand::{
    distributions::{Alphanumeric, Distribution},
    thread_rng,
};
use serde_json::json;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::env;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use walkdir::WalkDir;

const APP_CHART_NAME: &str = "b2b-dm-staging";
const INFRA_CHART_NAME: &str = "b2b-infra-dm-staging";
const ECR: &str = "560065381221.dkr.ecr.us-east-1.amazonaws.com";

pub fn tmp_dir() -> String {
    const LENGTH: usize = 10;

    let rng = thread_rng();
    let random_string: String = Alphanumeric.sample_iter(rng).take(LENGTH).collect();
    format!("dm-{}", random_string)
}

pub struct TmpDirGuard(PathBuf);

impl TmpDirGuard {
    pub fn new(tmp_dir: &PathBuf) -> Self {
        TmpDirGuard(tmp_dir.clone())
    }
}

impl Drop for TmpDirGuard {
    fn drop(&mut self) {
        match fs::remove_dir_all(&self.0) {
            Ok(_) => (),
            Err(err) => eprintln!("remove_dir_all error: {:?}", err),
        };
    }
}

#[derive(Debug, Clone)]
pub struct Override {
    pub key: String,
    pub value: String,
}

impl TryFrom<&str> for Override {
    type Error = &'static str;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let parts = s.split('=').collect::<Vec<_>>();
        if parts.len() == 2 {
            Ok(Override {
                key: parts[0].to_string(),
                value: parts[1].to_string(),
            })
        } else {
            Err("Malformed environment key-value pair, should be similar to FOO=bar")
        }
    }
}

impl ToString for Override {
    fn to_string(&self) -> String {
        format!("{}={}", self.key, self.value)
    }
}

pub fn whereis(cmd: &str) -> Option<String> {
    env::var("PATH")
        .expect("get PATH")
        .split(':')
        .fold(None, |acc, path| {
            if acc.is_some() {
                acc
            } else {
                let full_path = Path::new(path).join(cmd);
                if full_path.is_file() {
                    full_path.to_str().map(ToString::to_string)
                } else {
                    None
                }
            }
        })
}

pub fn calc_app_checksum(work_dir: &PathBuf, args: &[&str]) -> io::Result<String> {
    calc_chart_checksum(&charts_app_dir(work_dir, APP_CHART_NAME), args)
}

pub fn calc_infra_checksum(work_dir: &PathBuf, args: &[&str]) -> io::Result<String> {
    calc_chart_checksum(&charts_app_dir(work_dir, INFRA_CHART_NAME), args)
}

pub fn calc_chart_checksum(work_dir: &PathBuf, args: &[&str]) -> io::Result<String> {
    let mut hasher = Sha256::new();
    for entry in WalkDir::new(work_dir).into_iter() {
        let ent = entry?;
        let metadata = ent.metadata()?;
        if metadata.is_file() {
            let mut file = fs::File::open(ent.path())?;
            let mut contents = String::new();
            file.read_to_string(&mut contents)?;
            hasher.update(contents.as_bytes());
        }
    }
    hasher.update(args.iter().fold(String::new(), |mut acc, x| {
        acc.push_str(x);
        acc
    }));
    Ok(hex::encode(hasher.finalize()))
}

pub fn print_utils_version() {
    println!(
        "utils sha256: {}",
        String::from_utf8(version().to_vec()).expect("get version")
    );
}

pub fn version() -> &'static [u8] {
    // version file can create so:
    // $ git rev-parse HEAD > src/version
    include_bytes!("version")
}

pub fn print_command_result(output: Output) {
    println!("status: {}", output.status);
    println!(
        "{}",
        String::from_utf8(output.stdout).expect("valid stdout")
    );
    eprintln!(
        "{}",
        String::from_utf8(output.stderr).expect("valid stderr")
    );
}

pub fn clone_and_prepare_repo(work_dir: &PathBuf) -> bool {
    let mut results = vec![];
    let output = Command::new("git")
        .args(&[
            "clone",
            "--recursive",
            "--depth=1",
            "git@github.com:Aviora/b2b-helm.git",
            ".",
        ])
        .current_dir(work_dir)
        .output()
        .expect("can not clone repo");
    results.push(output.status.success());
    print_command_result(output);

    let options = dir::CopyOptions::new();
    let app_chars_path = Path::join(work_dir, "charts/apps/b2b-dm-staging/charts");
    let infra_chars_path = Path::join(work_dir, "charts/apps/b2b-infra-dm-staging/charts");
    results.push(
        fs::create_dir(&app_chars_path)
            .map(|_| true)
            .unwrap_or_else(|_| false),
    );
    results.push(
        fs::create_dir(&infra_chars_path)
            .map(|_| true)
            .unwrap_or_else(|_| false),
    );
    results.push(
        dir::copy(
            charts_lib_dir(work_dir, "b2b-app"),
            app_chars_path,
            &options,
        )
        .map(|_| true)
        .unwrap_or_else(|_| false),
    );
    let infra_charts = [
        "b2b-elasticsearch",
        "b2b-kafka",
        "b2b-kafka-zk",
        "b2b-kibana",
        "b2b-redis",
        "b2b-postgres",
    ];
    for chart in infra_charts.iter() {
        results.push(
            dir::copy(charts_lib_dir(work_dir, chart), &infra_chars_path, &options)
                .map(|_| true)
                .unwrap_or_else(|_| false),
        );
    }
    results.iter().all(|x| *x)
}

pub fn print_sha256_repo(work_dir: &Path) -> bool {
    let output = Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .current_dir(&work_dir)
        .output()
        .expect("get hash of HEAD");
    let success = output.status.success();
    println!(
        "b2b-helm sha256: {}",
        String::from_utf8(output.stdout).expect("get sha256 of HEAD")
    );
    success
}

pub fn create_infra_atrs(domain: &str, namespace: &str, name: &str, checksum: &str) -> Vec<String> {
    let chart = INFRA_CHART_NAME;
    let rn = release_name(chart, name);
    let db = rn.replace("-", "_");
    let es = "elasticsearch";

    vec![
        "upgrade",
        "--install",
        "--namespace",
        namespace,
        &rn,
        &app_dir(chart),
        "--set",
        &format!("global.deploy_checksum={}", checksum),
        "--set",
        &format!("b2b-kafka-int.zk={}-zk-0.{}-zk.{}/int", rn, rn, namespace),
        "--set",
        &format!(
            "b2b-{}.cluster_hosts={}-{}-0.{}-{}.{}",
            es, rn, es, rn, es, namespace
        ),
        "--set",
        &format!("b2b-postgres.postgres_db={}", db),
        "--set",
        &format!("global.staging_name={}", name),
        "--set",
        &format!(
            "b2b-kibana.elasic_hosts=http://{}-{}-{}-0.{}-{}-{}.{}:9200",
            chart, name, es, chart, name, es, namespace
        ),
        "--set",
        &format!("b2b-kibana.domain=kibana.{}.{}", name, domain),
        "--wait",
        "--timeout",
        "600",
        "--debug",
    ]
    .iter()
    .map(ToString::to_string)
    .collect::<Vec<_>>()
}

pub fn create_app_atrs(
    domain: &str,
    namespace: &str,
    name: &str,
    tag: &str,
    app_env_overrides: Vec<Override>,
    staging_overrides: Vec<Override>,
    checksum: &str,
) -> Vec<String> {
    let rn = release_name(APP_CHART_NAME, name);
    let rni = release_name(INFRA_CHART_NAME, name);
    let es = "elasticsearch";

    let mut atrs = vec![
        "upgrade",
        "--install",
        "--namespace",
        namespace,
        &rn,
        &app_dir(APP_CHART_NAME),
        "--set",
        &format!("global.deploy_checksum={}", checksum),
        "--set",
        &format!("global.image_prefix={}", ECR),
        "--set",
        &format!("global.image_tag={}", tag),
        "--set",
        &format!("b2b-app.email_domain={}.{}", name, domain),
        "--set",
        &format!("b2b-app.domain={}.{}", name, domain),
        "--set",
        &format!(
            "b2b-app.connections.pg_instance=avia:avia@{}-postgres-0.{}-postgres.{}:5432",
            rni, rni, namespace
        ),
        "--set",
        &format!(
            "b2b-app.connections.elastic=http://{}-{}.{}:9200",
            rni, es, namespace
        ),
        "--set",
        &format!(
            "b2b-app.connections.elasic_hosts=http://{}-{}-0.{}-{}.{}:9200",
            rni, es, rni, es, namespace
        ),
        "--set",
        &format!(
            "b2b-app.connections.redis={}-redis-0.{}-redis.{}:6379",
            rni, rni, namespace
        ),
        "--set",
        &format!(
            "b2b-app.connections.kafka_ext={}-kafka-int-0.{}-kafka-int:9092",
            rni, rni
        ),
        "--set",
        &format!(
            "b2b-app.connections.kafka_int={}-kafka-int-0.{}-kafka-int:9092",
            rni, rni
        ),
        "--set",
        &format!("global.staging_name={}", name),
        "--wait",
        "--timeout",
        "600",
        "--debug",
    ]
    .iter()
    .map(ToString::to_string)
    .collect::<Vec<_>>();

    let mut app_atrs = app_env_overrides
        .into_iter()
        .map(|e| {
            vec![
                "--set".to_string(),
                format!("b2b-app.env.{}", e.to_string()),
            ]
        })
        .flatten()
        .collect::<Vec<_>>();
    atrs.append(&mut app_atrs);

    let mut staging_atrs = staging_overrides
        .into_iter()
        .map(|e| vec!["--set".to_string(), e.to_string()])
        .flatten()
        .collect::<Vec<_>>();
    atrs.append(&mut staging_atrs);

    atrs
}

pub fn delete_infra_atrs(name: &str) -> Vec<String> {
    vec!["delete", &release_name(INFRA_CHART_NAME, name), "--purge"]
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
}

pub fn delete_app_atrs(name: &str) -> Vec<String> {
    vec!["delete", &release_name(APP_CHART_NAME, name), "--purge"]
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
}

pub fn delete_pvcs_atrs(namespace: &str, name: &str) -> Vec<String> {
    vec![
        "delete",
        "pvc",
        "-n",
        namespace,
        "-l",
        &format!("staging={}", name),
    ]
    .iter()
    .map(ToString::to_string)
    .collect::<Vec<_>>()
}

pub fn delete_cert_atrs(namespace: &str, name: &str) -> Vec<String> {
    vec![
        "delete",
        "certificate",
        "-n",
        namespace,
        &format!("b2b-dm-staging-{}-tls", name),
        &format!("b2b-dm-staging-{}-tls-tasker", name),
        &format!("b2b-infra-dm-staging-{}-kibana-tls", name),
    ]
    .iter()
    .map(ToString::to_string)
    .collect::<Vec<_>>()
}

pub fn check_list(namespace: &str, name: &str) -> String {
    json!({
        "Deployments": names_to_check_list(deployment_names(name), namespace),
        "StatefulSets": names_to_check_list(statefulset_names(name), namespace),
    })
    .to_string()
}

fn release_name(chart_name: &str, name: &str) -> String {
    format!("{}-{}", chart_name, name)
}

fn deployment_names(name: &str) -> Vec<String> {
    vec![
        format!("b2b-dm-staging-{}-app", name),
        format!("b2b-dm-staging-{}-cron", name),
        format!("b2b-dm-staging-{}-tasker", name),
        format!("b2b-infra-dm-staging-{}-kibana", name),
    ]
}

fn statefulset_names(name: &str) -> Vec<String> {
    vec![
        format!("b2b-infra-dm-staging-{}-elasticsearch", name),
        format!("b2b-infra-dm-staging-{}-kafka-int", name),
        format!("b2b-infra-dm-staging-{}-postgres", name),
        format!("b2b-infra-dm-staging-{}-redis", name),
        format!("b2b-infra-dm-staging-{}-zk", name),
    ]
}

fn names_to_check_list(names: Vec<String>, namespace: &str) -> Vec<HashMap<String, String>> {
    names
        .iter()
        .map(|n| {
            let mut h = HashMap::new();
            h.insert("ResourceName".to_string(), n.to_string());
            h.insert("Namespace".to_string(), namespace.to_string());
            h
        })
        .collect::<Vec<_>>()
}

fn charts_lib_dir(work_dir: &PathBuf, lib_name: &str) -> PathBuf {
    Path::join(work_dir, lib_dir(lib_name))
}

fn charts_app_dir(work_dir: &PathBuf, app_name: &str) -> PathBuf {
    Path::join(work_dir, app_dir(app_name))
}

fn lib_dir(lib_name: &str) -> String {
    format!("charts/libs/{}", lib_name)
}

fn app_dir(app_name: &str) -> String {
    format!("charts/apps/{}", app_name)
}
