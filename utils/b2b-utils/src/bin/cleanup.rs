use clap::{App, Arg};
use std::process::Command;

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

    let output = Command::new("kubectl")
        .args(delete_pvcs_atrs(namespace, name))
        .output()
        .expect("could not delete PVCs");
    print_command_result(output);

    let output = Command::new("kubectl")
        .args(delete_cert_atrs(namespace, name))
        .output()
        .expect("could not delete certificates");
    print_command_result(output);

    Ok(())
}
