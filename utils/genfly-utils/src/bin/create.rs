use clap::{App, Arg};
use std::convert::TryInto;
use std::fs;
use std::path::Path;
use std::process::{exit, Command};

use genfly_utils::*;

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
            Arg::with_name("env")
                .long("env")
                .short("e")
                .required(false)
                .multiple(true)
                .takes_value(true),
        )
        .get_matches();

    let project_name = matches.value_of("project-name").expect("get project-name");
    let base_domain = matches.value_of("base-domain").expect("get base-domain");
    let namespace = matches.value_of("namespace").expect("get namepace");
    let name = matches.value_of("name").expect("get name");
    let tag = matches.value_of("tag").expect("get tag");
    let envs = matches
        .values_of("env")
        .unwrap_or_else(Default::default)
        .map(|e| e.try_into().expect("get valid key=value"))
        .collect::<Vec<EnvPair>>();

    println!("project_name: {:?}", project_name);
    println!("base_domain: {:?}", base_domain);
    println!("namespace: {:?}", namespace);
    println!("name: {:?}", name);
    println!("tag: {:?}", tag);
    println!("envs: {:?}", envs);

    let b2b_heml = whereis("b2b-helm").expect("get b2b-helm path");

    let tmp_dir = tmp_dir();
    let work_dir = Path::new("/tmp").join(tmp_dir);
    println!("use {:?} dir", work_dir);

    let _guard = TmpDirGuard::new(&work_dir);
    fs::create_dir(&work_dir)?;

    let output = Command::new("git")
        .args(&[
            "clone",
            "--recursive",
            "--depth=1",
            "git@github.com:Aviora/b2b-helm.git",
            ".",
        ])
        .current_dir(&work_dir)
        .output()
        .expect("clone repo");
    let success = output.status.success();
    print_command_result(output);

    fs::copy(&b2b_heml, Path::new(&work_dir).join("b2b-helm"))?;

    let output = Command::new("b2b-helm")
        .args(create_infra_atrs(base_domain, namespace, name))
        .current_dir(&work_dir)
        .output()
        .expect("create infra");
    let success2 = output.status.success();
    print_command_result(output);

    let output = Command::new("b2b-helm")
        .args(create_app_atrs(base_domain, namespace, name, tag, envs))
        .current_dir(&work_dir)
        .output()
        .expect("create app");
    let success3 = output.status.success();
    print_command_result(output);

    if !(success && success2 && success3) {
        exit(1)
    }

    Ok(())
}
