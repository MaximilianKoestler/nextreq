use std::collections::HashMap;
use std::io::{self, Write};

use nextreq::formula::Formula;

fn prompt(text: &str) -> Option<String> {
    println!("{}:", text);
    print!("    > ");

    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let input = input.trim().to_owned();

    if input.is_empty() {
        None
    } else {
        Some(input)
    }
}

fn main() {
    let mut vars: HashMap<String, f64> = HashMap::new();

    loop {
        println!();
        let input = prompt("Enter a formula");

        if input.is_none() {
            println!("Quitting");
            return;
        }
        let input = input.unwrap();

        let result = Formula::new(&input).and_then(|f| f.eval_with(&vars));
        match result {
            Ok(val) => {
                println!("ans = {}", val);
                vars.insert("ans".to_owned(), val);
            }
            Err(err) => println!("{}", err),
        }
    }
}
