use clap::{App, Arg};
use serde_json::json;
use std::io::Write;
use std::process::{exit, Command, Stdio};

const KUBEDOG_TIMEOUT: usize = 3;

fn main() -> std::io::Result<()> {
    let matches = App::new("check")
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
        .get_matches();

    let _project_name = matches
        .value_of("project-name")
        .expect("could not get project-name");
    let _base_domain = matches
        .value_of("base-domain")
        .expect("could not get base-domain");
    let namespace = matches
        .value_of("namespace")
        .expect("could not get namepace");
    let name = matches.value_of("name").expect("could not get name");
    let _tag = matches.value_of("tag").expect("could not get tag");

    let kubedog_stdin = json!({
        "Deployments": [{"ResourceName": format!("app-{}", name), "Namespace": namespace}]
    })
    .to_string();

    let mut child = Command::new("kubedog")
        .args(&["multitrack", "-t", &KUBEDOG_TIMEOUT.to_string()])
        .stdin(Stdio::piped())
        .spawn()
        .expect("failed to call kubedog");

    {
        let stdin = child.stdin.as_mut().expect("failed to open stdin");
        stdin
            .write_all(kubedog_stdin.as_bytes())
            .expect("failed to write to stdin");
    }

    let output = child.wait_with_output().expect("failed to read stdout");
    let success = output.status.success();

    if !success {
        exit(1)
    }

    Ok(())
}
