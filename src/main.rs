fn main() -> std::io::Result<()> {
    let path;
    match std::env::args().nth(1) {
        Some(x) => { path = x; }
        None => {
            println!("Usage: {} <input path>", std::env::args().nth(0).unwrap());
            std::process::exit(1);
        }
    }
    let input = std::fs::read_to_string(path)?;
    mydecaf::run(&input, &mut std::io::stdout())
}
