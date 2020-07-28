use clap::{App, Arg};
use std::process::{exit, Command};

use b2b_utils::*;

fn main() -> std::io::Result<()> {
    let matches = App::new("delete")
        .version("0.1")
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
        .get_matches();

    let namespace = matches
        .value_of("namespace")
        .expect("could not get namepace");
    let name = matches.value_of("name").expect("could not get name");

    print_utils_version();

    println!("namespace: {:?}", namespace);
    println!("name: {:?}", name);

    let output = Command::new("helm")
        .args(helm_app_release_status_atrs(name))
        .output()
        .expect("could not get status app release");
    let success = output.status.success();
    print_command_result(output);

    let output = Command::new("helm")
        .args(helm_infra_release_status_atrs(name))
        .output()
        .expect("could not get status infra release");
    let success2 = output.status.success();
    print_command_result(output);

    if !(success && success2) {
        exit(1)
    }

    Ok(())
}
