use crate::lexer::Token;
use crate::parser::ASTNode;
use std::collections::HashMap;

pub struct Interpreter {
    context: HashMap<String, i64>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            context: HashMap::new(),
        }
    }

    pub fn visit(&mut self, node: &ASTNode) -> i64 {
        match node {
            ASTNode::Num(value) => *value,
            ASTNode::Var(name) => *self.context.get(name).expect("Undefined variable"),
            ASTNode::BinOp(left, op, right) => {
                let left_val = self.visit(left);
                let right_val = self.visit(right);
                match op {
                    Token::Plus => left_val + right_val,
                    Token::Minus => left_val - right_val,
                    Token::Multiply => left_val * right_val,
                    Token::Divide => left_val / right_val,
                    _ => panic!("Unexpected operator: {:?}", op),
                }
            }
            ASTNode::Assign(name, expr) => {
                let value = self.visit(expr);
                self.context.insert(name.clone(), value);
                value
            }
            ASTNode::Echo(expr) => {
                let value = self.visit(expr);
                println!("{}", value); // Print the value for echo statements
                value
            }
        }
    }

    pub fn interpret(&mut self, node: ASTNode) -> i64 {
        self.visit(&node)
    }
}

