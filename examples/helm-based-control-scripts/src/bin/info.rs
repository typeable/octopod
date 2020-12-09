use clap::{App, Arg};

fn main() -> std::io::Result<()> {
    let matches = App::new("cleanup")
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
        .get_matches();

    let _project_name = matches
        .value_of("project-name")
        .expect("could not get project-name");
    let base_domain = matches
        .value_of("base-domain")
        .expect("could not get base-domain");
    let _namespace = matches
        .value_of("namespace")
        .expect("could not get namepace");
    let name = matches.value_of("name").expect("could not get name");

    [("app", format!("https://{}.{}", name, base_domain))]
        .iter()
        .for_each(|(key, value)| println!("{},{}", key, value));

    Ok(())
}
