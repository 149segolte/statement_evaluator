use sentence::{SentenceTokenizer, Token};

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Operator(String),
    Operand(String),
}

fn remove_punctuation(string: &str) -> String {
    let mut result = Vec::new();
    let tokenizer = SentenceTokenizer::new();
    let tokens = tokenizer.tokenize(string);
    for t in tokens.iter() {
        match t {
            Token::Word(ref word) => {
                result.push(word.clone());
            },
            _ => (),
        }
    }
    result.join(" ").trim().to_string()
}

fn expression(list: &mut Vec<String>, tokens: &Vec<Token>) -> Vec<String> {
    let mut intermidiate = Vec::new();
    let mut operand = String::new();
    for t in tokens.iter() {
        match t {
            Token::Word(ref word) => {
                if word == "and" || word == "or" || word == "If" || word == "then" {
                    if !operand.is_empty() {
                        intermidiate.push(Type::Operand(remove_punctuation(&operand)));
                        operand.clear();
                    }
                    intermidiate.push(Type::Operator(word.to_string()));
                } else {
                    operand.push_str(" ");
                    operand.push_str(word);
                }
            },
            _ => {
                operand.push_str(" ");
                operand.push_str(&to_string(&vec![t.clone()]));
            },
        }
    }
    if !operand.is_empty() {
        intermidiate.push(Type::Operand(remove_punctuation(&operand)));
        operand.clear();
    }
    if intermidiate.contains(&Type::Operator("If".to_string())) {
        let mut stack = Vec::new();
        for stuff in intermidiate.iter() {
            match stuff {
                Type::Operand(ref operand) => {
                    stack.push(operand.clone());
                },
                Type::Operator(ref operator) => {
                    if operator == "then" {
                        let mut then = Vec::new();
                        while let Some(s) = stack.pop() {
                            if s == "If" {
                                break;
                            }
                            then.push(s);
                        }
                        then.reverse();
                        stack.push(then);
                    } else {
                        stack.push(operator.clone());
                    }
                },
            }
        }
    }
    println!("{:?}", intermidiate);
    todo!()
}

fn to_string(tokens: &Vec<Token>) -> String {
    let mut string = String::new();
    for t in tokens.iter() {
        match t {
            Token::Word(ref word) => {
                string.push_str(" ");
                string.push_str(word);
            },
            Token::Punctuation(ref punct) => {
                match punct {
                    sentence::Punctuation::Colon => string.push_str(":"),
                    sentence::Punctuation::Comma => string.push_str(","),
                    sentence::Punctuation::Dash => string.push_str("-"),
                    sentence::Punctuation::Exclamation => string.push_str("!"),
                    sentence::Punctuation::Period => string.push_str("."),
                    sentence::Punctuation::Question => string.push_str("?"),
                    sentence::Punctuation::Semicolon => string.push_str(";"),
                }
            }
            _ => (),
        }
    }
    string.trim().to_string()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: ./{} <text/file path>", args[0]);
        println!("This program takes one argument, either a multi-line string or a file path");
        return;
    }
    let text = if args[1].ends_with(".txt") {
        std::fs::read_to_string(&args[1]).unwrap()
    } else {
        args[1].clone()
    };

    let input = text.lines().take(3).collect::<Vec<&str>>();

    println!("Input:");
    for line in input.iter() {
        println!("{}", line);
    }
    println!();

    let tokenizer = SentenceTokenizer::new();
    let mut tokens = input.iter().map(|line| {
        let token = tokenizer.tokenize(line);
        token
    }).collect::<Vec<Vec<Token>>>();

    let mut argument = Vec::new();
    for (i, line) in tokens.iter().enumerate() {
        match line[0] {
            Token::Word(ref word) => {
                if word == "Therefore" || word == "So" {
                    argument = line.clone();
                    argument.remove(0);
                    if let Some(Token::Punctuation(_)) = argument.first() {
                        argument.remove(0);
                    }
                    tokens.remove(i);
                    break;
                }
            },
            _ => {},
        }
    }
    if argument.is_empty() {
        println!("No argument found");
        return;
    }

    println!("F1: {}", to_string(&tokens[0]));
    println!("F2: {}", to_string(&tokens[1]));
    println!("G: {}", to_string(&argument));
    println!();

    let mut operands = Vec::new();
    let f1 = expression(&mut operands, &tokens[0]);
    let f2 = expression(&mut operands, &tokens[1]);
    let g = expression(&mut operands, &argument);

    println!("{:?}", tokens);
}
