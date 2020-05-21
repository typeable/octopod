use clap::{App, Arg};
use std::convert::TryInto;
use std::fs;
use std::path::Path;
use std::process::Command;

use b2b_utils::*;

fn main() -> std::io::Result<()> {
    let matches = App::new("create")
        .version("0.1")
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

    let base_domain = matches.value_of("base-domain").expect("get base-domain");
    let namespace = matches.value_of("namespace").expect("get namepace");
    let name = matches.value_of("name").expect("get name");
    let tag = matches.value_of("tag").expect("get tag");
    let envs = matches
        .values_of("env")
        .expect("get envs")
        .map(|e| e.try_into().expect("get valid key=value"))
        .collect::<Vec<EnvPair>>();

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
    println!(
        "{}",
        String::from_utf8(output.stdout).expect("valid stdout")
    );
    eprintln!(
        "{}",
        String::from_utf8(output.stderr).expect("valid stderr")
    );

    fs::copy(&b2b_heml, Path::new(&work_dir).join("b2b-helm"))?;

    let output = Command::new("b2b-helm")
        .args(create_infra_atrs(base_domain, namespace, name))
        .current_dir(&work_dir)
        .output()
        .expect("create infra");
    println!(
        "{}",
        String::from_utf8(output.stdout).expect("valid stdout")
    );
    eprintln!(
        "{}",
        String::from_utf8(output.stderr).expect("valid stderr")
    );

    let output = Command::new("b2b-helm")
        .args(create_app_atrs(base_domain, namespace, name, tag, envs))
        .current_dir(&work_dir)
        .output()
        .expect("create app");
    println!(
        "{}",
        String::from_utf8(output.stdout).expect("valid stdout")
    );
    eprintln!(
        "{}",
        String::from_utf8(output.stderr).expect("valid stderr")
    );

    Ok(())
}
