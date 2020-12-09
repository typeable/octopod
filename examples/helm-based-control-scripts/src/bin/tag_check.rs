use clap::{App, Arg};
use serde_derive::Deserialize;
use std::process::exit;

static REGISTRY: &str = "typeable";
static REPOSITORY: &str = "octopod-web-app-example";

#[derive(Debug, Deserialize)]
struct Resp {
    results: Vec<Tag>,
}

#[derive(Debug, Deserialize)]
struct Tag {
    name: String,
}

async fn do_request() -> reqwest::Result<Resp> {
    let url = format!(
        "https://hub.docker.com/v2/repositories/{}/{}/tags",
        REGISTRY, REPOSITORY
    );
    let body = reqwest::get(&url).await?.json::<Resp>().await?;
    Ok(body)
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let matches = App::new("tag_check")
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
    let _namespace = matches
        .value_of("namespace")
        .expect("could not get namepace");
    let _name = matches.value_of("name").expect("could not get name");
    let tag = matches.value_of("tag").expect("could not get tag");

    let mut tag_found = false;
    match do_request().await {
        Ok(resp) => tag_found = resp.results.iter().any(|t| t.name == tag),
        Err(err) => eprintln!("could not get tags, reason: {:?}", err),
    }

    if !tag_found {
        exit(1)
    }

    Ok(())
}
