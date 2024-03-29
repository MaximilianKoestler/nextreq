use std::io::{self, Write};

use nextreq::formula::error::ErrorPosition;
use nextreq::formula::{Formula, VariableDict};

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
    let mut vars: VariableDict = VariableDict::new();

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
                match val {
                    nextreq::formula::Value::Number(v) => {
                        vars.insert("ans".to_owned(), v);
                    }
                    nextreq::formula::Value::Literal(_) => {}
                };
            }
            Err(err) => {
                let input_length = input.chars().count();

                let start = match err.start {
                    ErrorPosition::End => input_length - 1,
                    ErrorPosition::Offset(position) => position,
                };
                let length = match (err.start, err.end) {
                    (ErrorPosition::Offset(start), ErrorPosition::Offset(end)) => end - start,
                    (ErrorPosition::Offset(start), ErrorPosition::End) => input_length - start,
                    (ErrorPosition::End, ErrorPosition::Offset(_)) => {
                        panic!("error end before error start")
                    }
                    (ErrorPosition::End, ErrorPosition::End) => 1,
                };
                let indentation = (0..=(start + 5)).map(|_| ' ').collect::<String>();
                let marker = (0..length).map(|_| '^').collect::<String>();
                println!("{}{}", indentation, marker);
                println!("{}", err.error);
            }
        }
    }
}
