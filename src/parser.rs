use crate::lexer::{Lexer, Token};

#[derive(Debug, Clone)]
pub enum ASTNode {
    Num(i64),
    Var(String),
    BinOp(Box<ASTNode>, Token, Box<ASTNode>),
    Assign(String, Box<ASTNode>),
    Echo(Box<ASTNode>), // Added for echo statements
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let token = lexer.get_next_token();
        Parser {
            lexer,
            current_token: token,
        }
    }

    fn eat(&mut self, token: Token) {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&token) {
            self.current_token = self.lexer.get_next_token();
        } else {
            panic!(
                "Unexpected token: {:?}, expected: {:?}",
                self.current_token, token
            );
        }
    }

    fn factor(&mut self) -> ASTNode {
        let current_token = self.current_token.clone();

        match current_token {
            Token::Number(value) => {
                self.eat(Token::Number(value));
                ASTNode::Num(value)
            }
            Token::Identifier(name) => {
                let name_clone = name.clone();
                self.eat(Token::Identifier(name));
                ASTNode::Var(name_clone)
            }
            Token::LParen => {
                self.eat(Token::LParen);
                let node = self.expr();
                self.eat(Token::RParen);
                node
            }
            _ => panic!("Unexpected token: {:?}", current_token),
        }
    }

    fn term(&mut self) -> ASTNode {
        let mut node = self.factor();

        while matches!(self.current_token, Token::Multiply | Token::Divide) {
            let token = self.current_token.clone();
            if matches!(token, Token::Multiply) {
                self.eat(Token::Multiply);
            } else {
                self.eat(Token::Divide);
            }
            node = ASTNode::BinOp(Box::new(node), token, Box::new(self.factor()));
        }

        node
    }

    fn expr(&mut self) -> ASTNode {
        let mut node = self.term();

        while matches!(self.current_token, Token::Plus | Token::Minus) {
            let token = self.current_token.clone();
            if matches!(token, Token::Plus) {
                self.eat(Token::Plus);
            } else {
                self.eat(Token::Minus);
            }
            node = ASTNode::BinOp(Box::new(node), token, Box::new(self.term()));
        }

        node
    }

    fn echo_statement(&mut self) -> ASTNode {
        self.eat(Token::Identifier("echo".to_string())); // Match 'echo' keyword
        let expr = self.expr(); // Expression to echo
        ASTNode::Echo(Box::new(expr))
    }

    fn assignment(&mut self) -> ASTNode {
        if let Token::Identifier(name) = &self.current_token {
            let name_clone = name.clone();
            self.eat(Token::Identifier(name_clone.clone()));
            self.eat(Token::Assign);
            ASTNode::Assign(name_clone, Box::new(self.expr()))
        } else {
            self.expr() // Default to an expression if not an assignment
        }
    }

    pub fn parse(&mut self) -> ASTNode {
        // Parse a single statement
        match self.current_token {
            Token::Identifier(ref id) if id == "echo" => self.echo_statement(),
            _ => self.assignment(), // Default to assignment or expression
        }
    }
}

