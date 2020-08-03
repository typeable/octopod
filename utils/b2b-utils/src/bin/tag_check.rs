use clap::{App, Arg};
use rusoto_ecr::{Ecr, EcrClient, ImageIdentifier, ListImagesFilter, ListImagesRequest};
use rusoto_signature::region::Region;
use std::process::exit;

use b2b_utils::*;

static REPOSITORY_NAME: &str = "default";
const MAX_RESULTS: i64 = 100;

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

    print_utils_version();

    println!("project_name: {:?}", project_name);
    println!("base_domain: {:?}", base_domain);
    println!("namespace: {:?}", namespace);
    println!("name: {:?}", name);
    println!("tag: {:?}", tag);

    let client = EcrClient::new(Region::UsEast1);
    let mut next_token = None;
    let mut tag_found = false;
    loop {
        let request = build_request(next_token.clone());
        let response = client.list_images(request).await;
        if let Ok(list_images_response) = response {
            next_token = list_images_response.next_token;
            if let Some(image_ids) = list_images_response.image_ids {
                if find_image(tag, &image_ids).is_some() {
                    tag_found = true;
                }
            }
        }
        if tag_found || next_token.is_none() {
            break;
        }
    }

    println!("tag found: {}", tag_found);

    if !tag_found {
        exit(1)
    }

    Ok(())
}

fn build_request(next_token: Option<String>) -> ListImagesRequest {
    ListImagesRequest {
        filter: Some(ListImagesFilter {
            tag_status: Some("TAGGED".to_string()),
        }),
        max_results: Some(MAX_RESULTS),
        next_token,
        registry_id: None,
        repository_name: REPOSITORY_NAME.to_string(),
    }
}

fn find_image<'a>(tag: &'a str, image_ids: &'a [ImageIdentifier]) -> Option<&'a ImageIdentifier> {
    image_ids
        .iter()
        .find(|image| image.image_tag.as_ref().map(|t| t == tag) == Some(true))
}
