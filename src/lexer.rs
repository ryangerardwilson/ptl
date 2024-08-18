// src/lexer.rs

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(i64),
    Identifier(String),
    Plus,
    Minus,
    Multiply,
    Divide,
    Assign,
    LParen,
    RParen,
    EOF,
}

/// The lexer will read the source code and convert it into a series of tokens.
#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            current_char: None,
        };
        l.current_char = l.input.chars().next();
        l
    }

    fn advance(&mut self) {
        self.position += 1;
        if self.position > self.input.len() - 1 {
            self.current_char = None;
        } else {
            self.current_char = self.input.chars().nth(self.position);
        }
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_some() && self.current_char.unwrap().is_whitespace() {
            self.advance();
        }
    }

    fn number(&mut self) -> Token {
        let mut number_str = String::new();
        while let Some(c) = self.current_char {
            if c.is_digit(10) {
                number_str.push(c);
                self.advance();
            } else {
                break;
            }
        }
        Token::Number(number_str.parse::<i64>().unwrap())
    }

    fn identifier(&mut self) -> Token {
        let mut id_str = String::new();
        while let Some(c) = self.current_char {
            if c.is_alphanumeric() {
                id_str.push(c);
                self.advance();
            } else {
                break;
            }
        }
        Token::Identifier(id_str)
    }

    pub fn get_next_token(&mut self) -> Token {
        while let Some(c) = self.current_char {
            match c {
                ' ' | '\t' | '\n' | '\r' => self.skip_whitespace(),
                '+' => {
                    self.advance();
                    return Token::Plus;
                }
                '-' => {
                    self.advance();
                    return Token::Minus;
                }
                '*' => {
                    self.advance();
                    return Token::Multiply;
                }
                '/' => {
                    self.advance();
                    return Token::Divide;
                }
                '=' => {
                    self.advance();
                    return Token::Assign;
                }
                '(' => {
                    self.advance();
                    return Token::LParen;
                }
                ')' => {
                    self.advance();
                    return Token::RParen;
                }
                _ => {
                    if c.is_digit(10) {
                        return self.number();
                    } else if c.is_alphabetic() {
                        return self.identifier();
                    } else {
                        panic!("Invalid character: {}", c);
                    }
                }
            }
        }
        Token::EOF
    }
}
