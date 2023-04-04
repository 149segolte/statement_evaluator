use std::{cell::RefCell, rc::Rc};

const ARGUMENTS: [&str; 2] = ["therefore", "so"];
const PLACEHOLDER: &str = "op";

type FilterFn = dyn Fn(&Filter, &Vec<Filter>, &mut Vec<String>, &[Type]) -> Option<Type>;

#[derive(Clone)]
struct Filter {
    words: Vec<String>,
    resultant: Vec<String>,
    func: Rc<RefCell<FilterFn>>,
}

impl Filter {
    fn new(words: Vec<String>, resultant: Vec<String>, func: Rc<RefCell<FilterFn>>) -> Self {
        Self {
            words,
            resultant,
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
    fn type_is(&self) -> String {
        match self {
            Type::Keyword(_) => "keyword".to_string(),
            Type::Operand(_) => "operand".to_string(),
            Type::Intermidiate(_) => "intermidiate".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expression {
    Operator(String),
    Operand(String),
    Parenthesis(String),
}

fn string_to_type(keywords: &Vec<String>, exp: &str) -> Result<Vec<Type>, String> {
    let mut res = Vec::new();
    let mut operand = String::new();
    let mut count = 0;
    let tokens = exp.split_whitespace().collect::<Vec<&str>>();

    for t in tokens.iter() {
        if keywords.contains(&t.to_string()) {
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
        match operands.iter().position(|x| x == sentence) {
            Some(x) => x.to_string(),
            None => {
                operands.push(sentence.to_string());
                id.to_string()
            }
        }
    } else {
        let filter = filters.remove(0);
        let keywords = filter
            .words
            .clone()
            .into_iter()
            .filter(|x| x != PLACEHOLDER)
            .collect::<Vec<String>>();
        let mut tmp = string_to_type(&keywords, sentence).unwrap();
        if tmp.len() > 1 {
            let mut flag = true;
            while flag {
                tmp = tmp
                    .windows(filter.words.len())
                    .map(|x| {
                        (*filter.func.borrow_mut())(&filter, &filters, operands, x)
                            .unwrap_or(x[0].clone())
                    })
                    .collect();
                flag = tmp.iter().any(|x| match x {
                    Type::Keyword(_) => true,
                    _ => false,
                });
            }
            let res = tmp[0].clone();
            match res {
                Type::Operand(ref op) => {
                    let id = operands.len() as usize;
                    operands.push(op.to_string());
                    id.to_string()
                }
                Type::Intermidiate(ref op) => op.to_string(),
                _ => unreachable!(),
            }
        } else {
            sentence_to_exp(&filters, operands, sentence)
        }
    }
}

fn from_string(resultants: &Vec<Vec<String>>, string: &str) -> Vec<Expression> {
    let symbols = resultants
        .iter()
        .flatten()
        .cloned()
        .filter(|x| x != PLACEHOLDER)
        .collect::<Vec<String>>();
    let res = string
        .split_whitespace()
        .map(|x| {
            let mut word = x.to_string();
            let mut res = Vec::new();
            while word.starts_with("(") {
                res.push("(".to_string());
                word = word[1..].to_string();
            }
            let mut tmp = Vec::new();
            while word.ends_with(")") {
                word = word[..word.len() - 1].to_string();
                tmp.push(")".to_string());
            }
            res.push(word);
            res.extend(tmp);
            res
        })
        .flatten()
        .map(|x| match x.as_str() {
            "(" => Expression::Parenthesis("(".to_string()),
            ")" => Expression::Parenthesis(")".to_string()),
            x if symbols.contains(&x.to_string()) => Expression::Operator(x.to_string()),
            _ => Expression::Operand(x.to_string()),
        })
        .collect::<Vec<Expression>>();
    res
}

fn precedence(resultants: &Vec<Vec<String>>, op: &str) -> i32 {
    resultants
        .iter()
        .flatten()
        .cloned()
        .filter(|x| x != PLACEHOLDER)
        .position(|x| x == op)
        .unwrap() as i32
}

fn infix_to_postfix(resultants: &Vec<Vec<String>>, infix: &str) -> String {
    let expression = from_string(resultants, infix);
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
                        && precedence(resultants, op)
                            <= precedence(resultants, stack.last().unwrap())
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

fn evaluate(
    resultants: &Vec<Vec<String>>,
    postfix: String,
    state: u32,
) -> (Vec<String>, Vec<bool>) {
    let expression = from_string(resultants, &postfix);
    let mut stack = Vec::new();
    let mut res = Vec::new();
    let mut header = Vec::new();

    for c in expression.iter() {
        match c {
            Expression::Operand(ref x) => {
                let bit = x.parse::<u32>().unwrap();
                let val = ((state >> bit) & 1) != 0;
                stack.push((x.clone(), val))
            }
            Expression::Operator(ref op) => {
                let no_of_operands = resultants.iter().find(|x| x.contains(op)).unwrap().len() - 1;
                let mut operands = Vec::new();
                let mut expr = String::new();
                match no_of_operands {
                    1 => {
                        expr.push_str(&format!("{}{}", op, stack.last().unwrap().0));
                        operands.push(stack.pop().unwrap());
                    }
                    2 => {
                        expr.push_str(&format!(
                            "({}{}{})",
                            stack.last().unwrap().0,
                            op,
                            stack[stack.len() - 2].0
                        ));
                        operands.push(stack.pop().unwrap());
                        operands.push(stack.pop().unwrap());
                    }
                    3 => {
                        expr.push_str(&format!(
                            "({}{}{}{}{})",
                            stack.last().unwrap().0,
                            op,
                            stack[stack.len() - 2].0,
                            op,
                            stack[stack.len() - 3].0
                        ));
                        operands.push(stack.pop().unwrap());
                        operands.push(stack.pop().unwrap());
                        operands.push(stack.pop().unwrap());
                    }
                    _ => panic!("Invalid number of operands"),
                }
                let val = match op.as_str() {
                    "~" => !operands[0].1,
                    "∧" => operands[1].1 & operands[0].1,
                    "∨" => operands[1].1 | operands[0].1,
                    "→" => !operands[1].1 | operands[0].1,
                    "↔" => operands[1].1 == operands[0].1,
                    "←" => !operands[0].1 | operands[1].1,
                    "⊕" => operands[1].1 ^ operands[0].1,
                    "↑" => !(operands[1].1 & operands[0].1),
                    "↓" => !(operands[1].1 | operands[0].1),
                    "⊤" => true,
                    "⊥" => false,
                    _ => panic!("Unknown operator {}", op),
                };
                header.push(expr.clone());
                res.push(val);
                stack.push((expr.clone(), val));
            }
            _ => panic!("Unknown expression {:?}", expression),
        }
    }
    (header, res)
}

fn filter_template(
    filter: &Filter,
    filters: &Vec<Filter>,
    operands: &mut Vec<String>,
    x: &[Type],
) -> Option<Type> {
    let pattern = filter
        .words
        .iter()
        .zip(x.iter())
        .map(|(a, b)| {
            if a == PLACEHOLDER {
                if b.type_is() == "keyword" {
                    false
                } else {
                    true
                }
            } else {
                b == &Type::Keyword(a.to_string())
            }
        })
        .collect::<Vec<bool>>();
    let flag = pattern.iter().all(|x| *x);
    if flag {
        let input_count = filter.words.iter().filter(|x| *x == PLACEHOLDER).count();
        let output_count = filter
            .resultant
            .iter()
            .filter(|x| *x == PLACEHOLDER)
            .count();
        let ops = if input_count > output_count {
            let mut res = Vec::new();
            let mut remaining = Vec::new();
            for item in x.iter() {
                if item.type_is() == "intermediate" {
                    res.push(item.clone());
                } else if item.type_is() == "operand" {
                    remaining.push(item.clone());
                }
            }
            let diff = output_count - res.len();
            res.extend(remaining.iter().take(diff - 1).cloned());
            let last = remaining
                .iter()
                .skip(diff - 1)
                .cloned()
                .filter_map(|x| match x {
                    Type::Operand(op) => Some(op),
                    _ => None,
                })
                .collect::<Vec<String>>();
            res.push(Type::Operand(last.join(" ")));
            res
        } else {
            x.iter()
                .filter(|x| x.type_is() != "keyword")
                .cloned()
                .collect::<Vec<Type>>()
        };
        let mut ops = ops
            .iter()
            .filter_map(|x| match x {
                Type::Intermidiate(ref exp) => Some(exp.clone()),
                Type::Operand(ref op) => Some(sentence_to_exp(&filters, operands, op)),
                Type::Keyword(_) => return None,
            })
            .collect::<Vec<String>>();
        let symbols = filter
            .resultant
            .iter()
            .map(|x| {
                if x == PLACEHOLDER {
                    ops.remove(0)
                } else {
                    x.to_string()
                }
            })
            .collect::<Vec<String>>();
        if symbols.len() == 2 {
            Some(Type::Intermidiate(symbols.join(" ")))
        } else {
            Some(Type::Intermidiate(format!("({})", symbols.join(" "))))
        }
    } else {
        None
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        println!("Usage: ./{} <path to vocabolary> <text/file path>", args[0]);
        println!("This program takes one argument, either a multi-line string or a file path");
        return;
    }
    let text = if args[2].ends_with(".txt") {
        std::fs::read_to_string(&args[2]).unwrap()
    } else {
        args[2].clone()
    };

    let input = text.lines().take(3).collect::<Vec<&str>>();

    let vocabolary: Vec<Vec<Vec<String>>> =
        serde_json::from_str(&std::fs::read_to_string(&args[1]).unwrap()).unwrap();

    let mut filters = Vec::new();
    for sentence in vocabolary.into_iter() {
        let words = sentence[0].clone();
        let res = sentence[1].clone();
        filters.push(Filter::new(
            words,
            res,
            Rc::new(RefCell::new(filter_template)),
        ));
    }

    println!("Input:");
    for line in input.iter() {
        println!("{}", line);
    }
    println!();

    let expressions = input
        .iter()
        .map(|line| {
            line.chars()
                .filter_map(|c| match c {
                    'a'..='z' | 'A'..='Z' => Some(c.to_lowercase().to_string()),
                    ' ' => Some(c.to_string()),
                    _ => None,
                })
                .collect::<String>()
                .split_whitespace()
                .collect::<Vec<&str>>()
                .join(" ")
        })
        .collect::<Vec<String>>();

    let (mut argument, statements): (Vec<String>, Vec<String>) =
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
    } else {
        argument = argument[0]
            .split_whitespace()
            .collect::<Vec<&str>>()
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        argument.remove(0);
    }

    let mut operands = Vec::new();
    let f1 = sentence_to_exp(&filters, &mut operands, &statements[0]);
    let f2 = sentence_to_exp(&filters, &mut operands, &statements[1]);
    let g = sentence_to_exp(&filters, &mut operands, &argument.join(" "));

    println!("Operands:");
    for (i, operand) in operands.iter().enumerate() {
        println!("{} => {}", i, operand);
    }
    println!();

    println!("F1: {}", f1);
    println!("F2: {}", f2);
    println!("G: {}", g);

    let final_expression = format!("(({}) ∧ ({})) → ({})", f1, f2, g);
    println!("Final expression: {}", final_expression);
    println!();

    let resultants = filters
        .iter()
        .map(|x| x.resultant.clone())
        .collect::<Vec<Vec<String>>>();
    let postfix = infix_to_postfix(&resultants, &final_expression);
    println!("Postfix: {}", postfix);
    println!("Truth table:");
    let no_of_operands = operands.len() as u32;
    let mut tt = Vec::new();
    let mut head = Vec::new();
    for i in 0..2_u32.pow(no_of_operands) {
        let mut state = (0..no_of_operands)
            .map(|j| (i >> j) & 1 != 0)
            .collect::<Vec<bool>>();
        let (top, mut res) = evaluate(&resultants, postfix.clone(), i);
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
        header.push(format!("{}", ((i + 48) as u8) as char));
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
