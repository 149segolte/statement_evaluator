use sentence::{SentenceTokenizer, Token};

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Operator(u32, String),
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
            }
            _ => (),
        }
    }
    result.join(" ").trim().to_string()
}

fn convertor(list: &mut Vec<String>, operand: &String) -> String {
    let tokens = SentenceTokenizer::new().tokenize(operand);
    let mut intermidiate = Vec::new();
    let mut operand = String::new();
    let mut count = 0;

    for t in tokens.iter() {
        match t {
            Token::Word(ref word) => {
                if word == "and" || word == "or" {
                    if !operand.is_empty() {
                        intermidiate.push(Type::Operand(remove_punctuation(&operand)));
                        operand.clear();
                    }
                    intermidiate.push(Type::Operator(count, word.to_string()));
                    count += 1;
                } else {
                    operand.push_str(" ");
                    operand.push_str(word);
                }
            }
            _ => {
                operand.push_str(" ");
                operand.push_str(&to_string(&vec![t.clone()]));
            }
        }
    }
    if !operand.is_empty() {
        intermidiate.push(Type::Operand(remove_punctuation(&operand)));
        operand.clear();
    }

    let mut operands = intermidiate
        .iter()
        .filter_map(|x| match x {
            Type::Operand(ref operand) => Some(operand.clone()),
            _ => None,
        })
        .collect::<Vec<String>>();
    operands.dedup();
    for op in operands.iter() {
        if !list.contains(op) {
            list.push(op.clone());
        }
    }

    intermidiate = intermidiate
        .iter()
        .filter_map(|x| match x {
            Type::Operand(ref operand) => {
                let index = list.iter().position(|x| x == operand).unwrap();
                let char = (index + 65) as u8;
                Some(Type::Operand(String::from_utf8(vec![char]).unwrap()))
            }
            _ => Some(x.clone()),
        })
        .collect();

    let res = intermidiate.iter().map(|x| match x {
        Type::Operand(ref operand) => operand.clone(),
        Type::Operator(_, ref op) => {
            if op == "and" {
                "&".to_string()
            } else {
                "|".to_string()
            }
        }
    }).collect::<Vec<String>>().join("");

    if res.len() > 2 {
        format!("({})", res)
    } else {
        res
    }
}

fn expression(list: &mut Vec<String>, tokens: &Vec<Token>) -> String {
    if tokens.contains(&Token::Word("If".to_string())) {
        let mut intermidiate = Vec::new();
        let tmp = implies(&mut intermidiate, tokens);
        let mut express = String::new();
        for char in tmp.chars() {
            match char {
                'A'..='Z' => {
                    let operand = intermidiate[char as usize - 65].clone();
                    express.push_str(&convertor(list, &operand));
                }
                _ => express.push(char),
            }
        }
        express
    } else {
        let input = remove_punctuation(&to_string(tokens));
        convertor(list, &input)
    }
}

fn implies(list: &mut Vec<String>, tokens: &Vec<Token>) -> String {
    let mut intermidiate = Vec::new();
    let mut operand = String::new();
    let mut count = 0;

    for t in tokens.iter() {
        match t {
            Token::Word(ref word) => {
                if word == "If" || word == "then" {
                    if !operand.is_empty() {
                        intermidiate.push(Type::Operand(remove_punctuation(&operand)));
                        operand.clear();
                    }
                    intermidiate.push(Type::Operator(count, word.to_string()));
                    count += 1;
                } else {
                    operand.push_str(" ");
                    operand.push_str(word);
                }
            }
            _ => {
                operand.push_str(" ");
                operand.push_str(&to_string(&vec![t.clone()]));
            }
        }
    }
    if !operand.is_empty() {
        intermidiate.push(Type::Operand(remove_punctuation(&operand)));
        operand.clear();
    }

    let mut operands = intermidiate
        .iter()
        .filter_map(|x| match x {
            Type::Operand(ref operand) => Some(operand.clone()),
            _ => None,
        })
        .collect::<Vec<String>>();
    operands.dedup();
    list.append(&mut operands.clone());
    intermidiate = intermidiate
        .iter()
        .filter_map(|x| match x {
            Type::Operand(ref operand) => {
                let index = operands.iter().position(|x| x == operand).unwrap();
                let char = (index + 65) as u8;
                Some(Type::Operand(String::from_utf8(vec![char]).unwrap()))
            }
            _ => Some(x.clone()),
        })
        .collect();

    let mut stack = Vec::new();
    let mut map = Vec::new();
    for t in intermidiate.iter() {
        match t {
            Type::Operator(ref id, ref op) => {
                if op == "If" {
                    stack.push(id.clone());
                } else if op == "then" {
                    map.push((stack.pop().unwrap(), id.clone()));
                }
            }
            _ => {}
        }
    }

    while !map.is_empty() {
        let (if_id, then_id) = map.remove(0);
        let then_index = intermidiate
            .iter()
            .position(|x| match x {
                Type::Operator(ref id, ref op) => id == &then_id && op == "then",
                _ => false,
            })
            .unwrap();
        if if_id + 1 == then_id {
            if let Type::Operand(_) = intermidiate[then_index + 1] {
                intermidiate.remove(then_index - 2);
                let a = intermidiate.remove(then_index - 2);
                intermidiate.remove(then_index - 2);
                let b = intermidiate.remove(then_index - 2);
                match (a, b) {
                    (Type::Operand(a), Type::Operand(b)) => {
                        intermidiate.insert(then_index - 2, Type::Operand(format!("({}={})", a, b)))
                    }
                    _ => panic!("Error"),
                }
            } else {
                map.push((if_id, then_id));
            }
        } else {
            map.push((if_id, then_id));
        }
    }

    match intermidiate.pop() {
        Some(Type::Operand(x)) => {
            let mut res = x.clone();
            res.remove(0);
            res.remove(res.len() - 1);
            res
        },
        _ => panic!("Error"),
    }
}

fn to_string(tokens: &Vec<Token>) -> String {
    let mut string = String::new();
    for t in tokens.iter() {
        match t {
            Token::Word(ref word) => {
                string.push_str(" ");
                string.push_str(word);
            }
            Token::Punctuation(ref punct) => match punct {
                sentence::Punctuation::Colon => string.push_str(":"),
                sentence::Punctuation::Comma => string.push_str(","),
                sentence::Punctuation::Dash => string.push_str("-"),
                sentence::Punctuation::Exclamation => string.push_str("!"),
                sentence::Punctuation::Period => string.push_str("."),
                sentence::Punctuation::Question => string.push_str("?"),
                sentence::Punctuation::Semicolon => string.push_str(";"),
            },
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
    let mut tokens = input
        .iter()
        .map(|line| {
            let token = tokenizer.tokenize(line);
            token
        })
        .collect::<Vec<Vec<Token>>>();

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
            }
            _ => {}
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

    println!("Operands:");
    for (i, operand) in operands.iter().enumerate() {
        println!("{} => {}", ((i + 65) as u8) as char, operand);
    }
    println!();

    println!("F1: {}", f1);
    println!("F2: {}", f2);
    println!("G: {}", g);
}
