use rand::{
    distributions::{Alphanumeric, Distribution},
    thread_rng,
};
use serde_json::json;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Output;

const APP_CHART_NAME: &str = "b2b-dm-staging";
const INFRA_CHART_NAME: &str = "b2b-infra-dm-staging";

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

#[derive(Debug)]
pub struct EnvPair {
    pub key: String,
    pub value: String,
}

impl TryFrom<&str> for EnvPair {
    type Error = &'static str;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let parts = s.split('=').collect::<Vec<_>>();
        if parts.len() == 2 {
            Ok(EnvPair {
                key: parts[0].to_string(),
                value: parts[1].to_string(),
            })
        } else {
            Err("Malformed environment key-value pair, should be similar to FOO=bar")
        }
    }
}

impl ToString for EnvPair {
    fn to_string(&self) -> String {
        format!("{}={}", self.key, self.value)
    }
}

pub fn create_infra_atrs(domain: &str, namespace: &str, name: &str) -> Vec<String> {
    let chart = INFRA_CHART_NAME;
    let rn = release_name(chart, name);
    let db = rn.replace("-", "_");
    let es = "elasticsearch";

    vec![
        "-d",
        "deploy",
        "--release-name",
        &rn,
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
        chart,
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
    envs: Vec<EnvPair>,
) -> Vec<String> {
    let rn = release_name(APP_CHART_NAME, name);
    let rni = release_name(INFRA_CHART_NAME, name);
    let es = "elasticsearch";

    let mut atrs = vec![
        "-d",
        "deploy",
        "--release-name",
        &rn,
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
    ]
    .iter()
    .map(ToString::to_string)
    .collect::<Vec<_>>();

    let mut env_atrs = envs
        .into_iter()
        .map(|e| {
            vec![
                "--set".to_string(),
                format!("b2b-app.env.{}", e.to_string()),
            ]
        })
        .flatten()
        .collect::<Vec<_>>();
    let mut tagged_release_name = vec![tagged_release(APP_CHART_NAME, tag)];

    atrs.append(&mut env_atrs);
    atrs.append(&mut tagged_release_name);

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

fn tagged_release(chart_name: &str, tag: &str) -> String {
    format!("{}:{}", chart_name, tag)
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
