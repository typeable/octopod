use clap::{App, Arg};
use std::process::{exit, Command};

use b2b_utils::*;

fn main() -> std::io::Result<()> {
    let matches = App::new("delete")
        .version("0.1")
        .arg(
            Arg::with_name("project-name")
                .long("project-name")
                .short("p")
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
        .get_matches();

    let project_name = matches
        .value_of("project-name")
        .expect("could not get project-name");
    let namespace = matches
        .value_of("namespace")
        .expect("could not get namepace");
    let name = matches.value_of("name").expect("could not get name");

    print_utils_version();

    println!("project_name: {:?}", project_name);
    println!("namespace: {:?}", namespace);
    println!("name: {:?}", name);

    let output = Command::new("helm")
        .args(delete_app_atrs(name))
        .output()
        .expect("could not delete app");
    let success = output.status.success();
    print_command_result(output);

    let output = Command::new("helm")
        .args(delete_infra_atrs(name))
        .output()
        .expect("could not delete infra");
    let success2 = output.status.success();
    print_command_result(output);

    if !(success && success2) {
        exit(1)
    }

    Ok(())
}
