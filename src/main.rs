use sentence::{SentenceTokenizer, Token};

const ARGUMENTS: [&str; 2] = ["therefore", "so"];

type FilterFn = fn(&Vec<Filter>, &mut Vec<String>, &[Type]) -> Option<Type>;

#[derive(Clone)]
struct Filter {
    length: usize,
    keywords: Vec<&'static str>,
    func: FilterFn,
}

impl Filter {
    fn new(length: usize, keywords: Vec<&'static str>, func: FilterFn) -> Self {
        Self {
            length,
            keywords,
            func,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Keyword(String),
    Operand(String),
    Intermidiate(String),
}

impl Type {
    fn to_string(&self) -> String {
        match self {
            Type::Keyword(k) => k.to_string(),
            Type::Operand(o) => o.to_string(),
            Type::Intermidiate(i) => i.to_string(),
        }
    }
}

enum Expression {
    Operator(String),
    Operand(String),
    Parenthesis(String),
}

fn string_to_type(keywords: &Vec<&str>, exp: &str) -> Result<Vec<Type>, String> {
    let mut res = Vec::new();
    let mut operand = String::new();
    let mut count = 0;
    let tokens = exp.split_whitespace().collect::<Vec<&str>>();

    for t in tokens.iter() {
        if keywords.contains(t) {
            if !operand.is_empty() {
                res.push(Type::Operand(operand.trim().to_string()));
                operand.clear();
            }
            res.push(Type::Keyword(t.to_string()));
            count += 1;
        } else {
            operand.push_str(" ");
            operand.push_str(t);
        }
    }
    if !operand.is_empty() {
        res.push(Type::Operand(operand.trim().to_string()));
        operand.clear();
    }
    let x = count as f32 / keywords.len() as f32;
    if x == (x as i32) as f32 {
        Ok(res)
    } else {
        Err("Invalid expression".to_string())
    }
}

fn sentence_to_exp(filters: &Vec<Filter>, operands: &mut Vec<String>, sentence: &str) -> String {
    let id = operands.len() as usize;
    let mut filters = filters.clone();
    if filters.is_empty() {
        operands.push(sentence.to_string());
        return id.to_string();
    } else {
        let filter = filters.remove(0);
        let mut tmp = string_to_type(&filter.keywords, sentence).unwrap();
        let mut flag = true;
        while flag {
            println!("{:?}", tmp);
            tmp = tmp
                .windows(filter.length)
                .map(|x| {
                    println!("x: {:?}", x);
                    for (i, f) in filters.iter().enumerate() {
                        println!("{}: {} {:?}", i, f.length, f.keywords);
                    }
                    println!("operands: {:?}", operands);
                    (filter.func)(&filters, operands, x).unwrap_or(x[0].clone())
                })
                .collect();
            println!("{:?}", tmp);
            flag = tmp.contains(&Type::Keyword(filter.keywords[0].to_string()));
        }
        todo!()
    }
}

/* fn token_to_expression(filters: Vec<Filter>, _operands: &mut Vec<String>, tokens: &str) -> String {
    let mut tmp = Vec::new();
    let mut intermidiate = Vec::new();
    for filter in filters.iter() {
        let keywords = filter.1.clone();
        let num = filter.0;
        tmp = string_to_type(&keywords, &tokens).unwrap_or(Vec::new());
        let mut flag = true;
        while flag {
            tmp = tmp
                .windows(num)
                .map(|x| match filter.2(x) {
                    Some(y) => {
                        intermidiate.append(
                            x.iter()
                                .filter_map(|op| match op {
                                    Type::Operand(ref statement) => {
                                        if !statement.starts_with("(") {
                                            Some(statement.to_string())
                                        } else {
                                            None
                                        }
                                    }
                                    _ => None,
                                })
                                .collect::<Vec<_>>()
                                .as_mut(),
                        );
                        y
                    }
                    None => x[0].to_owned(),
                })
                .collect::<Vec<_>>();
            flag = tmp.iter().any(|x| match x {
                Type::Operator(_, _) => true,
                _ => false,
            });
        }
        println!("{:?}", tmp);
        println!("{:?}", intermidiate);
    }
    todo!()
} */

fn from_string(string: &str) -> Vec<Expression> {
    let mut res = Vec::new();
    for c in string.chars() {
        match c {
            'A'..='Z' => res.push(Expression::Operand(c.to_string())),
            '∧' | '∨' | '~' | '=' => res.push(Expression::Operator(c.to_string())),
            '(' | ')' => res.push(Expression::Parenthesis(c.to_string())),
            _ => (),
        }
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
            Expression::Operand(ref val) => res.push_str(&format!("{} ", val)),
            Expression::Parenthesis(ref bracket) => match bracket.as_str() {
                "(" => stack.push("("),
                ")" => {
                    while !stack.is_empty() && stack.last().unwrap() != &"(" {
                        res.push_str(&format!("{} ", stack.pop().unwrap()));
                    }
                    stack.pop();
                }
                _ => (),
            },
            Expression::Operator(ref op) => {
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
            Expression::Operand(ref x) => {
                let bit = (x.clone().pop().unwrap()) as u8 - 65;
                let val = ((state >> bit) & 1) != 0;
                stack.push((x.clone(), val))
            }
            Expression::Operator(ref op) => {
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
            _ => panic!("Unknown expression"),
        }
    }
    (header, res)
}

fn main() {
    let implies = |filters: &Vec<Filter>, operands: &mut Vec<String>, x: &[Type]| match x {
        [Type::Keyword(ref op1), ref a, Type::Keyword(ref op2), ref b] => {
            if op1 == "if" && op2 == "then" {
                let a_exp = match a {
                    Type::Intermidiate(ref exp) => exp.clone(),
                    Type::Operand(ref op) => sentence_to_exp(filters, operands, op),
                    Type::Keyword(_) => return None,
                };
                let b_exp = match b {
                    Type::Intermidiate(ref exp) => exp.clone(),
                    Type::Operand(ref op) => sentence_to_exp(filters, operands, op),
                    Type::Keyword(_) => return None,
                };
                Some(Type::Intermidiate(format!(
                    "({} {} {})",
                    a_exp, "=>", b_exp
                )))
            } else {
                None
            }
        }
        _ => None,
    };
    let filter2 = |filters: &Vec<Filter>, operands: &mut Vec<String>, x: &[Type]| match x {
        [Type::Keyword(ref op1), ref a, Type::Keyword(ref op2), ref b, Type::Keyword(ref op3)] => {
            if op1 == "either" && op2 == "or" && op3 == "but not both" {
                let a_exp = match a {
                    Type::Intermidiate(ref exp) => exp.clone(),
                    Type::Operand(ref op) => sentence_to_exp(filters, operands, op),
                    _ => panic!("Unknown expression"),
                };
                let b_exp = match b {
                    Type::Intermidiate(ref exp) => exp.clone(),
                    Type::Operand(ref op) => sentence_to_exp(filters, operands, op),
                    _ => panic!("Unknown expression"),
                };
                Some(Type::Intermidiate(format!("({} {} {})", a_exp, "⊕", b_exp)))
            } else {
                None
            }
        }
        _ => None,
    };
    let filters: Vec<Filter> = vec![
        Filter::new(4, vec!["if", "then"], implies),
        Filter::new(5, vec!["either", "or", "but not both"], filter2),
    ];

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
    let expressions = input
        .iter()
        .map(|line| {
            let token = tokenizer.tokenize(line);
            token
                .iter()
                .filter_map(|t| match t {
                    Token::Word(ref w) => Some(w.to_lowercase()),
                    _ => None,
                })
                .collect::<Vec<String>>()
                .join(" ")
        })
        .collect::<Vec<String>>();

    let (argument, statements): (Vec<String>, Vec<String>) =
        expressions.into_iter().partition(|e| {
            let valid_args = ARGUMENTS
                .into_iter()
                .filter(|a| e.starts_with(a.to_owned()))
                .collect::<Vec<&str>>();
            if valid_args.is_empty() {
                false
            } else {
                true
            }
        });

    if argument.is_empty() {
        println!("No argument found");
        return;
    } else if argument.len() > 1 {
        println!("More than one argument found");
        return;
    }

    let mut operands = Vec::new();
    let f1 = sentence_to_exp(&filters, &mut operands, &statements[0]);
    let f2 = sentence_to_exp(&filters, &mut operands, &statements[1]);
    let g = sentence_to_exp(&filters, &mut operands, &argument[0]);

    println!("Operands:");
    for (i, operand) in operands.iter().enumerate() {
        println!("{} => {}", i, operand);
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
        let a_result = a_state.iter().fold(0, |acc, &b| acc * 2 + b as u32);
        let b_result = b_state.iter().fold(0, |acc, &b| acc * 2 + b as u32);
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
    let separator = header
        .iter()
        .map(|x| "-".repeat(x.chars().count()))
        .collect::<Vec<String>>()
        .join("-+-");
    println!("+-{}-+", separator);
    println!("| {} |", header.join(" | "));
    println!("+-{}-+", separator);
    for row in tt.iter().rev() {
        let mut opt = Vec::new();
        for (i, col) in row.iter().enumerate() {
            let indent = header[i].chars().count();
            opt.push(format!(
                "{:^pad$}",
                if *col { 'T' } else { 'F' },
                pad = indent
            ));
        }
        println!("| {} |", opt.join(" | "));
    }
    println!("+-{}-+", separator);
}
