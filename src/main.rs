use logos::Logos;
use std::collections::HashMap;
use std::fs;
use std::env;
use ndarray::{Array2, Axis};
use std::fmt;
use rayon::prelude::*;
use std::sync::{Arc, Mutex};
use std::env::vars;

#[derive(Logos, Debug, PartialEq, Clone)]
enum Token {
    #[token("=")]
    Equals,

    #[token(".pr()")]
    Print,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    #[regex(r"\[[^]]+\]", priority = 2)]
    DataFrame,  // Match anything within square brackets

    #[regex(r"loop@[0-9]+")]
    Loop,  // Recognize sequential loop constructs

    #[regex(r"[0-9]*\.?[0-9]+", priority = 1)]
    Number,  // Match both integers and floats

    #[regex(r#""[^"]*""#, priority = 3)]
    StringLiteral,  // Match string literals

    #[token("{")]
    OpenBrace,

    #[token("}")]
    CloseBrace,

    #[token("+")]
    Plus,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token("pi{")]
    PipeStart,

    #[token("}pe")]
    PipeEnd,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*::[a-zA-Z_][a-zA-Z0-9_]*(::[a-zA-Z_][a-zA-Z0-9_]*)*")]
    Pipes,

    #[token(".splinter")]
    Splinter,

    #[token(".coalesce")]
    Coalesce,

    #[regex(r"[ \t\n\f]+", logos::skip)]  // Skip whitespace
    Whitespace,
}

#[derive(Debug, Clone)]
enum Value {
    Float(f64),
    Str(String),
    DataFrame(Array2<Value>, Array2<Value>),  // DataFrame and its transpose
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Float(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::DataFrame(df, tdf) => {
                writeln!(f, "DataFrame:")?;
                for row in df.outer_iter() {
                    for item in row.iter() {
                        write!(f, "{} ", item)?;
                    }
                    writeln!(f)?;
                }
                writeln!(f, "Transpose:")?;
                for row in tdf.outer_iter() {
                    for item in row.iter() {
                        write!(f, "{} ", item)?;
                    }
                    writeln!(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
enum ASTNode {
    Assignment { var: String, value: Expr },
    Print { var: String },
    Loop { iterations: usize, parallel: bool, body: Vec<ASTNode> },
    ExprPrint(Expr),
    Pipe { name: String, body: Vec<ASTNode> },
    Pipes { name: String, pipes: Vec<String> },
    Splinter { pipes: String, coalesce: bool },
}

#[derive(Debug, Clone)]
enum Expr {
    Value(Value),
    Var(String),
    Add(Box<Expr>, Box<Expr>),
}

fn parse_df_literal(slice: &str) -> (Array2<Value>, Array2<Value>) {
    let content = &slice[1..slice.len() - 1];  // Extract inner content from square brackets

    let rows: Vec<&str> = content.split(';').filter(|s| !s.trim().is_empty()).collect();  // Split by rows and filter out empty strings
    let parsed_rows: Vec<Vec<Value>> = rows.iter()
        .map(|row| row.split(',')
            .filter(|s| !s.trim().is_empty())  // Filter out empty strings
            .map(|cell| {
                if let Ok(num) = cell.trim().parse::<f64>() {
                    Value::Float(num)
                } else {
                    Value::Str(cell.trim().to_string())
                }
            })
            .collect())
        .collect();

    let nrows = parsed_rows.len();
    let ncols = if nrows == 0 { 0 } else { parsed_rows[0].len() };

    // Flatten the nested rows into a single vector
    let flat_data: Vec<Value> = parsed_rows.into_iter().flatten().collect();
    let df = Array2::from_shape_vec((nrows, ncols), flat_data).unwrap();
    let tdf = df.clone().reversed_axes();

    (df, tdf)
}

fn parse_expr(tokens: &mut logos::Lexer<Token>) -> Expr {
    let mut expr = match tokens.next() {
        Some(Ok(Token::Number)) => Expr::Value(Value::Float(tokens.slice().parse().unwrap())),
        Some(Ok(Token::Identifier)) => Expr::Var(tokens.slice().to_string()),
        Some(Ok(Token::StringLiteral)) => {
            let literal = tokens.slice().trim_matches('"').to_string();
            Expr::Value(Value::Str(literal))
        },
        unexpected => panic!("Unexpected token in expression: {:?}", unexpected),
    };

    while let Some(Ok(token)) = tokens.clone().next() {
        match token {
            Token::Plus => {
                tokens.next();  // Consume the plus sign
                let rhs = parse_expr(tokens);
                expr = Expr::Add(Box::new(expr), Box::new(rhs));
            },
            _ => break,
        }
    }

    expr
}

fn parse_pipe_body(tokens: &mut logos::Lexer<Token>) -> Vec<ASTNode> {
    let mut body = Vec::new();
    let mut nested_level = 1;  // We're already inside one Pipe

    while let Some(token) = tokens.next() {
        match token {
            Ok(Token::PipeStart) => {
                let body_content = parse_pipe_body(tokens);  // Recursively parse nested pipe
                body.push(ASTNode::Pipe { name: String::new(), body: body_content });
            },
            Ok(Token::PipeEnd) => {
                nested_level -= 1;
                if nested_level == 0 {
                    return body;
                }
            },
            Ok(Token::Identifier) => {
                let var = tokens.slice().to_string();
                if let Some(Ok(Token::Equals)) = tokens.clone().next() {
                    tokens.next();  // Consume Equals
                    let expr = parse_expr(tokens);
                    body.push(ASTNode::Assignment { var, value: expr });
                } else if let Some(Ok(Token::Print)) = tokens.clone().next() {
                    tokens.next();  // Consume Print
                    body.push(ASTNode::Print { var });
                }
            },
            Ok(other) => { /* Handle other tokens within the pipe body */ },
            Err(e) => eprintln!("Error token inside pipe body: {:?}", e),
        }
    }

    body
}


fn parse(tokens: &mut logos::Lexer<Token>) -> Vec<ASTNode> {
    let mut ast = Vec::new();
    while let Some(token) = tokens.next() {
        println!("Processing token: {:?}", token); // Debug print

        match token {
            Ok(Token::Identifier) => {
                let var = tokens.slice().to_string();
                if let Some(Ok(Token::Equals)) = tokens.clone().next() {
                    tokens.next(); // Consume Equals
                    if let Some(Ok(Token::PipeStart)) = tokens.next() {
                        let body = parse_pipe_body(tokens);
                        ast.push(ASTNode::Pipe { name: var, body });
                    } else if let Some(Ok(Token::Identifier)) = tokens.clone().next() {
                        let next_var = tokens.slice().to_string();
                        if next_var.contains("::") {
                            let pipes = next_var.split("::").map(String::from).collect();
                            ast.push(ASTNode::Pipes { name: var, pipes });
                        } else {
                            let expr = parse_expr(tokens);
                            ast.push(ASTNode::Assignment { var, value: expr });
                        }
                    } else {
                        let expr = parse_expr(tokens);
                        ast.push(ASTNode::Assignment { var, value: expr });
                    }
                } else if let Some(Ok(Token::Print)) = tokens.clone().next() {
                    tokens.next(); // Consume Print
                    ast.push(ASTNode::Print { var });
                } else if let Some(Ok(Token::Splinter)) = tokens.clone().next() {
                    tokens.next(); // Consume Splinter
                    let mut coalesce = false;
                    if let Some(Ok(Token::Coalesce)) = tokens.clone().next() {
                        tokens.next(); // Consume Coalesce
                        coalesce = true;
                    }
                    ast.push(ASTNode::Splinter { pipes: var.clone(), coalesce });
                    println!("Parsed Splinter: pipes={}, coalesce={}", var, coalesce);
                }
            },
            Ok(Token::Splinter) => {
                if let Some(Ok(Token::Identifier)) = tokens.next() {
                    let pipes_var = tokens.slice().to_string();
                    let coalesce = if let Some(Ok(Token::Coalesce)) = tokens.clone().next() {
                        tokens.next(); // Consume Coalesce
                        true
                    } else {
                        false
                    };
                    ast.push(ASTNode::Splinter { pipes: pipes_var.clone(), coalesce });
                    println!("Parsed Splinter: pipes={}, coalesce={}", pipes_var, coalesce);
                }
            },
            Ok(Token::Loop) => {
                let slice = tokens.slice();
                println!("Loop token slice: {}", slice);

                let iterations = slice[5..].parse::<usize>().unwrap();
                if let Some(Ok(Token::OpenBrace)) = tokens.next() {
                    let body = parse(tokens);
                    ast.push(ASTNode::Loop { iterations, parallel: false, body });
                } else {
                    eprintln!("Expected '{{' after loop declaration, found {:?}", tokens.next());
                }
            },
            Ok(Token::CloseBrace) => break,
            Ok(Token::StringLiteral) => {
                let literal = tokens.slice().trim_matches('"').to_string();
                if let Some(Ok(Token::Print)) = tokens.next() {
                    ast.push(ASTNode::ExprPrint(Expr::Value(Value::Str(literal))));
                }
            },
            Ok(token) => {
                eprintln!("Unexpected token in parse: {:?}", token);
            },
            Err(_) => {
                eprintln!("Error in lexer");
            }
        }
    }

    ast
}


fn slice_contains_double_colon(s: &str) -> bool {
    s.contains("::")
}


fn eval_expr(expr: Expr, variables: &HashMap<String, Value>) -> Value {
    match expr {
        Expr::Value(val) => val,
        Expr::Var(name) => variables.get(&name).cloned().expect(&format!("Undefined variable: {}", name)),
        Expr::Add(lhs, rhs) => {
            let lhs_val = eval_expr(*lhs, variables);
            let rhs_val = eval_expr(*rhs, variables);
            if let (Value::Float(lhs_num), Value::Float(rhs_num)) = (lhs_val, rhs_val) {
                Value::Float(lhs_num + rhs_num)
            } else {
                panic!("Invalid operands for addition");
            }
        }
    }
}

fn interpret(
    ast: Vec<ASTNode>,
    variables: &Arc<Mutex<HashMap<String, Value>>>,
    pipes: &Arc<Mutex<HashMap<String, Vec<ASTNode>>>>
) {
    println!("Interpreting AST with {} nodes", ast.len());
    for node in ast.iter() {  // Use iter() to borrow nodes
        println!("Interpreting node: {:?}", node);
        match node {
            ASTNode::Assignment { var, ref value } => {
                let val = eval_expr(value.clone(), &variables.lock().unwrap());
                println!("Assigning {} to {}", var, val);
                variables.lock().unwrap().insert(var.clone(), val);  // Use clone to avoid moving var
            },
            ASTNode::Print { var } => {
                let vars = variables.lock().unwrap();
                if let Some(value) = vars.get(var) {
                    println!(">>> {}", value);
                } else {
                    println!("Error: Variable '{}' is not defined", var);
                }
            },
            ASTNode::ExprPrint(ref expr) => {
                let vars = variables.lock().unwrap();
                let val = eval_expr(expr.clone(), &vars);  // Clone expr to preserve node
                println!(">>> {}", val);
            },
            ASTNode::Loop { iterations, parallel, ref body } => {
                if *parallel {
                    println!("Executing loop with parallel iterations: {}", iterations);
                    (0..*iterations).into_par_iter().for_each(|_| {
                        let local_vars = Arc::new(Mutex::new(variables.lock().unwrap().clone()));
                        let local_pipes = Arc::new(Mutex::new(pipes.lock().unwrap().clone()));
                        interpret(body.clone(), &local_vars, &local_pipes);
                    });
                } else {
                    println!("Executing loop with sequential iterations: {}", iterations);
                    for _ in 0..*iterations {
                        interpret(body.clone(), variables, pipes);
                    }
                }
            },
            ASTNode::Pipe { name, ref body } => {
                println!("Defining pipe: {}", name);
                let mut pipes_map = pipes.lock().unwrap();
                pipes_map.insert(name.clone(), body.clone());
                println!("Pipe '{}' defined with body: {:?}", name, body);
            },
            ASTNode::Pipes { name, pipes: ref pipe_names } => {
                println!("Storing pipes into: {}", name);
                // Lock scope minimized
                let all_nodes: Vec<ASTNode> = {
                    let pipes_map = pipes.lock().unwrap();
                    let mut nodes = Vec::new();
                    for pipe_name in pipe_names.iter() {
                        if let Some(pipe_content) = pipes_map.get(pipe_name) {
                            nodes.extend(pipe_content.clone());
                        } else {
                            panic!("Undefined pipe: {}", pipe_name);
                        }
                    }
                    nodes
                };
                println!("All nodes collected for '{}': {:?}", name, &all_nodes);
                {
                    let mut pipes_map = pipes.lock().unwrap();
                    pipes_map.insert(name.clone(), all_nodes);
                }
                println!("Stored pipes successfully for '{}'", name);
            },
            ASTNode::Splinter { pipes: ref pipe_name, coalesce } => {
                println!("Executing splintered pipes: {}", pipe_name);
                println!("Should wait for completion: {}", coalesce);

                // Lock scope minimized
                let pipes_to_execute = {
                    let pipes_map = pipes.lock().unwrap();
                    pipes_map.get(pipe_name).cloned()
                };

                if let Some(pipes_to_execute) = pipes_to_execute {
                    println!("Pipes to execute: {:?}", pipes_to_execute);

                    let handles: Vec<_> = pipes_to_execute.into_iter().map(|pipe_ast| {
                        let local_vars = Arc::clone(variables);
                        let local_pipes = Arc::clone(pipes);
                        let pipe_ast_clone = pipe_ast.clone();
                        std::thread::spawn(move || {
                            println!("Thread started for pipe AST: {:?}", pipe_ast_clone);
                            interpret(vec![pipe_ast], &local_vars, &local_pipes); // Ensure it's wrapped in a Vec
                            println!("Thread completed for pipe AST: {:?}", pipe_ast_clone);
                        })
                    }).collect();

                    if *coalesce {
                        println!("Join started for splintered pipes");
                        for handle in handles {
                            println!("Joining thread handle.");
                            match handle.join() {
                                Ok(_) => println!("Thread successfully joined."),
                                Err(e) => eprintln!("Thread join error: {:?}", e),
                            }
                        }
                        println!("Join completed for splintered pipes");
                    }
                } else {
                    println!("No pipes found for '{}'", pipe_name);
                    panic!("Undefined pipe: {}", pipe_name);
                }
            }
        }
        println!("Finished interpreting node: {:?}", node); // Ensure this prints for each node
    }
}



fn ast_from_string(pipe_str: &str) -> Vec<ASTNode> {
    let mut lexer = Token::lexer(pipe_str);
    parse(&mut lexer)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path_to_ptl_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let content = fs::read_to_string(file_path).expect("Could not read file");

    let mut lexer = Token::lexer(&content);
    let ast = parse(&mut lexer);

    println!("Parsed AST: {:?}", ast);

    let variables = Arc::new(Mutex::new(HashMap::new()));
    let pipes = Arc::new(Mutex::new(HashMap::new()));

    interpret(ast, &variables, &pipes);
}

