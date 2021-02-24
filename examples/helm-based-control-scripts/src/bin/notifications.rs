use clap::{App, Arg};

fn main() -> std::io::Result<()> {
    let matches = App::new("notifications")
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
            Arg::with_name("old-status")
                .long("old-status")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("new-status")
                .long("new-status")
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
    let _namespace = matches
        .value_of("namespace")
        .expect("could not get namepace");
    let _name = matches.value_of("name").expect("could not get name");
    let _tag = matches.value_of("tag").expect("could not get tag");
    let _old_status = matches
        .value_of("old-status")
        .expect("could not get old-status");
    let _new_status = matches
        .value_of("new-status")
        .expect("could not get new-status");

    // nop

    Ok(())
}
