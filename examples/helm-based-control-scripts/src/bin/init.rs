use std::env;

fn main() -> std::io::Result<()> {
    let _home = env::var("HOME").expect("could not get $HOME");

    // nop

    Ok(())
}
