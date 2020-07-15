use clap::{App, Arg};
use std::convert::TryInto;
use std::fs;
use std::path::Path;
use std::process::{exit, Command};

use b2b_utils::*;

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

    let project_name = matches
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
    let envs = matches
        .values_of("env")
        .unwrap_or_else(Default::default)
        .map(|e| e.try_into().expect("could not get valid key=value"))
        .collect::<Vec<EnvPair>>();

    print_utils_version();

    println!("project_name: {:?}", project_name);
    println!("base_domain: {:?}", base_domain);
    println!("namespace: {:?}", namespace);
    println!("name: {:?}", name);
    println!("tag: {:?}", tag);
    println!("envs: {:?}", envs);

    let tmp_dir = tmp_dir();
    let work_dir = Path::new("/tmp").join(tmp_dir);
    println!("use {:?} dir", work_dir);

    let _guard = TmpDirGuard::new(&work_dir);
    fs::create_dir(&work_dir)?;

    let success = clone_and_prepare_repo(&work_dir);
    let success2 = print_sha256_repo(&work_dir);

    let envs_str = envs.iter().fold(String::new(), |mut acc, x| {
        acc.push_str(&x.to_string());
        acc
    });
    let args = [project_name, base_domain, namespace, name, tag, &envs_str];
    let app_checksum = calc_app_checksum(&work_dir, &args)?;
    let infra_checksum = calc_infra_checksum(&work_dir, &args)?;

    println!("app checksum: {}", app_checksum);
    println!("infra checksum: {}", infra_checksum);

    let output = Command::new("helm")
        .args(create_infra_atrs(
            base_domain,
            namespace,
            name,
            &infra_checksum,
        ))
        .current_dir(&work_dir)
        .output()
        .expect("could not create infra");
    let success3 = output.status.success();
    print_command_result(output);

    let output = Command::new("helm")
        .args(create_app_atrs(
            base_domain,
            namespace,
            name,
            tag,
            envs,
            &app_checksum,
        ))
        .current_dir(&work_dir)
        .output()
        .expect("could not create app");
    let success4 = output.status.success();
    print_command_result(output);

    if !(success && success2 && success3 && success4) {
        exit(1)
    }

    Ok(())
}
