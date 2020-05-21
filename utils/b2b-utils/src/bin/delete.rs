use clap::{App, Arg};
use std::process::Command;

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

    let namespace = matches.value_of("namespace").expect("get namepace");
    let name = matches.value_of("name").expect("get name");
    println!("namespace: {:?}", namespace);
    println!("name: {:?}", name);

    let output = Command::new("helm")
        .args(delete_app_atrs(name))
        .output()
        .expect("delete app");
    println!(
        "{}",
        String::from_utf8(output.stdout).expect("valid stdout")
    );
    eprintln!(
        "{}",
        String::from_utf8(output.stderr).expect("valid stderr")
    );

    let output = Command::new("helm")
        .args(delete_infra_atrs(name))
        .output()
        .expect("delete infra");
    println!(
        "{}",
        String::from_utf8(output.stdout).expect("valid stdout")
    );
    eprintln!(
        "{}",
        String::from_utf8(output.stderr).expect("valid stderr")
    );

    let output = Command::new("kubectl")
        .args(delete_pvcs_atrs(namespace, name))
        .output()
        .expect("delete PVCs");
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
