use clap::{App, Arg};
use rand::{
    distributions::{Alphanumeric, Distribution},
    thread_rng,
};
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::{exit, Command};

static REGISTRY: &str = "typeable";
static IMAGE: &str = "octopod-web-app-example";
static GIT_REPOSITORY: &str = "https://github.com/typeable/octopod.git";

struct TmpDirGuard(PathBuf);

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
struct Override {
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

fn main() -> std::io::Result<()> {
    let matches = App::new("create")
        .version("0.1")
        .arg(
            Arg::with_name("project-name")
                .long("project-name")
                .short("p")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("base-domain")
                .long("base-domain")
                .short("d")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("namespace")
                .long("namespace")
                .short("s")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("name")
                .long("name")
                .short("n")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("tag")
                .long("tag")
                .short("t")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("app-env-override")
                .long("app-env-override")
                .short("e")
                .required(false)
                .multiple(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("deployment-override")
                .long("deployment-override")
                .short("o")
                .required(false)
                .multiple(true)
                .takes_value(true),
        )
        .get_matches();

    let _project_name = matches
        .value_of("project-name")
        .expect("could not get project-name");
    let base_domain = matches
        .value_of("base-domain")
        .expect("could not get base-domain");
    let namespace = matches
        .value_of("namespace")
        .expect("could not get namepace");
    let name = matches.value_of("name").expect("could not get name");
    let tag = matches.value_of("tag").expect("could not get tag");
    let app_env_overrides = matches
        .values_of("app-env-override")
        .unwrap_or_else(Default::default)
        .map(|e| e.try_into().expect("could not get valid key=value"))
        .collect::<Vec<Override>>();
    let deployment_overrides = matches
        .values_of("deployment-override")
        .unwrap_or_else(Default::default)
        .map(|e| e.try_into().expect("could not get valid key=value"))
        .collect::<Vec<Override>>();

    let tmp_dir = tmp_dir();
    let work_dir = Path::new("/tmp").join(tmp_dir);

    let _guard = TmpDirGuard::new(&work_dir);
    fs::create_dir(&work_dir)?;

    let success = clone_and_prepare_repo(&work_dir);

    let output = Command::new("helm")
        .args(command_args(
            base_domain,
            namespace,
            name,
            tag,
            app_env_overrides,
            deployment_overrides,
        ))
        .current_dir(&work_dir)
        .output()
        .expect("could not create app");
    let success2 = output.status.success();

    if !(success && success2) {
        exit(1)
    }

    Ok(())
}

fn tmp_dir() -> String {
    const LENGTH: usize = 10;

    let rng = thread_rng();
    let random_string: String = Alphanumeric.sample_iter(rng).take(LENGTH).collect();
    format!("octopod-{}", random_string)
}

fn clone_and_prepare_repo(work_dir: &PathBuf) -> bool {
    let output = Command::new("git")
        .args(&["clone", "--recursive", "--depth=1", GIT_REPOSITORY, "."])
        .current_dir(work_dir)
        .output()
        .expect("could not clone repo");
    output.status.success()
}

fn command_args(
    domain: &str,
    namespace: &str,
    name: &str,
    tag: &str,
    app_env_overrides: Vec<Override>,
    deployment_overrides: Vec<Override>,
) -> Vec<String> {
    let mut args = vec![
        "upgrade",
        "--install",
        "--namespace",
        namespace,
        &format!("app-{}", name),
        "examples/web-app/charts/web-app",
        "--set",
        &format!("image_prefix={}", REGISTRY),
        "--set",
        &format!("image={}", IMAGE),
        "--set",
        &format!("image_tag={}", tag),
        "--set",
        &format!("domain={}.{}", name, domain),
        "--wait",
        "--timeout",
        "300",
        "--debug",
    ]
    .iter()
    .map(ToString::to_string)
    .collect::<Vec<_>>();

    let mut app_args = app_env_overrides
        .into_iter()
        .map(|e| {
            vec![
                "--set".to_string(),
                format!("env.{}", e.to_string()),
            ]
        })
        .flatten()
        .collect::<Vec<_>>();
    args.append(&mut app_args);

    let mut deployment_args = deployment_overrides
        .into_iter()
        .map(|e| vec!["--set".to_string(), e.to_string()])
        .flatten()
        .collect::<Vec<_>>();
    args.append(&mut deployment_args);

    args
}
