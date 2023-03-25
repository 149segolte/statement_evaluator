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

fn dedup_operands(list: &mut Vec<String>, expression: &mut String) {
    let initial = list.clone();
    for operand in initial.iter() {
        let tokens = sentence::SentenceTokenizer::new().tokenize(operand);
        if tokens.contains(&Token::Word("not".to_string())) {
            list.remove(list.iter().position(|x| x == operand).unwrap());
            let op = tokens
                .iter()
                .filter(|x| x != &&Token::Word("not".to_string()))
                .map(|x| match x {
                    Token::Word(ref word) => word.clone(),
                    _ => "".to_string(),
                })
                .collect::<Vec<String>>()
                .join(" ");
            if !list.contains(&op) {
                list.push(op.clone());
            }
        }
    }
    let mut new_expression = String::new();
    for char in expression.chars() {
        match char {
            'A'..='Z' => {
                let index = char as u8 - 65;
                let op = initial[index as usize].clone();
                let tokens = sentence::SentenceTokenizer::new().tokenize(&op);
                if tokens.contains(&Token::Word("not".to_string())) {
                    let new_op = tokens
                        .iter()
                        .filter(|x| x != &&Token::Word("not".to_string()))
                        .map(|x| match x {
                            Token::Word(ref word) => word.clone(),
                            _ => "".to_string(),
                        })
                        .collect::<Vec<String>>()
                        .join(" ");
                    let new_index = list.iter().position(|x| x == &new_op).unwrap();
                    let new = (new_index as u8 + 65) as char;
                    new_expression.push_str(vec!['~', new].iter().collect::<String>().as_str());
                } else {
                    let new_index = list.iter().position(|x| x == &op).unwrap();
                    let new = (new_index as u8 + 65) as char;
                    new_expression.push(new);
                }
            }
            _ => new_expression.push(char),
        }
    }
    *expression = new_expression;
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

    let mut res = intermidiate
        .iter()
        .map(|x| match x {
            Type::Operand(ref operand) => operand.clone(),
            Type::Operator(_, ref op) => {
                if op == "and" {
                    " ∧ ".to_string()
                } else {
                    " ∨ ".to_string()
                }
            }
        })
        .collect::<Vec<String>>()
        .join("");

    dedup_operands(list, &mut res);

    if res.len() > 2 {
        format!("({})", res)
    } else {
        res
    }
}

fn token_to_expression(list: &mut Vec<String>, tokens: &Vec<Token>) -> String {
    let mut express = String::new();
    if tokens.contains(&Token::Word("If".to_string())) {
        let mut intermidiate = Vec::new();
        let tmp = implies(&mut intermidiate, tokens);
        for char in tmp.chars() {
            match char {
                'A'..='Z' => {
                    let operand = intermidiate[char as usize - 65].clone();
                    express.push_str(&convertor(list, &operand));
                }
                _ => express.push(char),
            }
        }
    } else {
        let input = remove_punctuation(&to_string(tokens));
        express.push_str(&convertor(list, &input));
    }
    express
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
                    (Type::Operand(a), Type::Operand(b)) => intermidiate
                        .insert(then_index - 2, Type::Operand(format!("({} = {})", a, b))),
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
        }
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

fn from_string(string: &str) -> Vec<Type> {
    let mut res = Vec::new();
    let mut count = 0;
    for c in string.chars() {
        match c {
            'A'..='Z' => res.push(Type::Operand(c.to_string())),
            '(' | ')' | '∧' | '∨' | '~' | '=' => res.push(Type::Operator(count, c.to_string())),
            _ => (),
        }
        count += 1;
    }
    res
}

fn precedence(op: &str) -> i32 {
    match op {
        "=" => 1,
        "∨" => 2,
        "∧" => 3,
        "~" => 4,
        _ => -1,
    }
}

fn infix_to_postfix(infix: &str) -> String {
    let expression = from_string(infix);
    let mut stack = Vec::new();
    let mut res = String::new();

    for c in expression.iter() {
        match c {
            Type::Operand(ref val) => res.push_str(&format!("{} ", val)),
            Type::Operator(_, ref op) => match op.as_str() {
                "(" => stack.push("("),
                ")" => {
                    while !stack.is_empty() && stack.last().unwrap() != &"(" {
                        res.push_str(&format!("{} ", stack.pop().unwrap()));
                    }
                    stack.pop();
                }
                _ => {
                    if stack.is_empty() || stack.last().unwrap() == &"(" {
                        stack.push(op);
                    } else {
                        while !stack.is_empty()
                            && stack.last().unwrap() != &"("
                            && precedence(op) <= precedence(stack.last().unwrap())
                        {
                            res.push_str(&format!("{} ", stack.pop().unwrap()));
                        }
                        stack.push(op);
                    }
                }
            },
        }
    }
    while !stack.is_empty() {
        res.push_str(&format!("{} ", stack.pop().unwrap()));
    }
    res
}

fn evaluate(postfix: String, state: u32) -> (Vec<String>, Vec<bool>) {
    let expression = from_string(&postfix);
    let mut stack = Vec::new();
    let mut res = Vec::new();
    let mut header = Vec::new();

    for c in expression.iter() {
        match c {
            Type::Operand(ref x) => {
                let bit = (x.clone().pop().unwrap()) as u8 - 65;
                let val = ((state >> bit) & 1) != 0;
                stack.push((x.clone(), val))
            }
            Type::Operator(_, ref op) => {
                let mut operands = Vec::new();
                let mut expr = String::new();
                if op == "~" {
                    operands.push(stack.pop().unwrap());
                    expr.push_str(op);
                    expr.push_str(&operands[0].0);
                } else {
                    operands.push(stack.pop().unwrap());
                    operands.push(stack.pop().unwrap());
                    expr.push_str("(");
                    expr.push_str(&operands[1].0);
                    expr.push_str(op);
                    expr.push_str(&operands[0].0);
                    expr.push_str(")");
                }
                let val = match op.as_str() {
                    "~" => !operands[0].1,
                    "∧" => operands[1].1 & operands[0].1,
                    "∨" => operands[1].1 | operands[0].1,
                    "=" => !operands[1].1 | operands[0].1,
                    _ => panic!("Unknown operator"),
                };
                header.push(expr.clone());
                res.push(val);
                stack.push((expr.clone(), val));
            }
        }
    }
    (header, res)
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

    let mut operands = Vec::new();
    let f1 = token_to_expression(&mut operands, &tokens[0]);
    let f2 = token_to_expression(&mut operands, &tokens[1]);
    let g = token_to_expression(&mut operands, &argument);

    println!("Operands:");
    for (i, operand) in operands.iter().enumerate() {
        println!("{} => {}", ((i + 65) as u8) as char, operand);
    }
    println!();

    println!("F1: {}", f1.replace("=", "=>"));
    println!("F2: {}", f2.replace("=", "=>"));
    println!("G: {}", g.replace("=", "=>"));

    let final_expression = format!(
        "(({}) ∧ ({})) => {}",
        f1.replace("=", "=>"),
        f2.replace("=", "=>"),
        g.replace("=", "=>")
    );
    println!("Final expression: {}", final_expression);
    println!();

    let postfix = infix_to_postfix(&final_expression);
    println!("Postfix: {}", postfix);
    println!("Truth table:");
    let no_of_operands = operands.len() as u32;
    let mut tt = Vec::new();
    let mut head = Vec::new();
    for i in 0..2_u32.pow(no_of_operands) {
        let mut state = (0..no_of_operands)
            .map(|j| (i >> j) & 1 != 0)
            .collect::<Vec<bool>>();
        let (top, mut res) = evaluate(postfix.clone(), i);
        head = top;
        state.append(&mut res);
        tt.push(state);
    }
    tt.sort_by(|a, b| {
        let a_state = a[0..operands.len()].to_vec();
        let b_state = b[0..operands.len()].to_vec();
        let a_result = a_state.iter().fold(0, |acc, &b| acc*2 + b as u32);
        let b_result = b_state.iter().fold(0, |acc, &b| acc*2 + b as u32);
        a_result.cmp(&b_result)
    });

    // print truth table in a nice way
    let mut header = Vec::new();
    for i in 0..operands.len() {
        header.push(format!("{}", ((i + 65) as u8) as char));
    }
    for expr in head.iter() {
        header.push(format!("{}", expr.replace("=", "=>")));
    }
    let separator = header.iter().map(|x| "-".repeat(x.chars().count())).collect::<Vec<String>>().join("-+-");
    println!("+-{}-+", separator);
    println!("| {} |", header.join(" | "));
    println!("+-{}-+", separator);
    for row in tt.iter().rev() {
        let mut opt = Vec::new();
        for (i, col) in row.iter().enumerate() {
            let indent = header[i].chars().count();
            opt.push(format!("{:^pad$}", if *col { 'T' } else { 'F' }, pad = indent));
        }
        println!("| {} |", opt.join(" | "));
        println!("+-{}-+", separator);
    }
}
