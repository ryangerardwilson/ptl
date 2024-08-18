mod interpreter;
mod lexer;
mod parser;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::{ASTNode, Parser};
use std::env;
use std::fs;

/// Run interpreter on a given source string
pub fn run(source: &str, print_ast: bool) -> i64 {
    let lexer = Lexer::new(source.to_string());
    let mut parser = Parser::new(lexer);

    // Parse the source to get the ASTNode
    let ast = parser.parse();

    // Optionally print the AST
    if print_ast {
        print_ast_tree(&ast, 0); // Pass the single ASTNode and indent
    }

    // Create an Interpreter and pass the parsed ASTNode to interpret
    let mut interpreter = Interpreter::new();
    interpreter.interpret(ast) // Pass the ASTNode here
}


fn print_ast_tree(node: &ASTNode, indent: usize) {
    match node {
        ASTNode::Num(value) => {
            println!("{}Num({})", " ".repeat(indent), value);
        }
        ASTNode::Var(name) => {
            println!("{}Var({})", " ".repeat(indent), name);
        }
        ASTNode::BinOp(left, op, right) => {
            println!("{}BinOp", " ".repeat(indent));
            println!("{}  Operator: {:?}", " ".repeat(indent + 2), op);
            print_ast_tree(left, indent + 2);
            print_ast_tree(right, indent + 2);
        }
        ASTNode::Assign(name, expr) => {
            println!("{}Assign", " ".repeat(indent));
            println!("{}  Variable: {}", " ".repeat(indent + 2), name);
            print_ast_tree(expr, indent + 2);
        }
        ASTNode::Echo(expr) => {
            println!("{}Echo", " ".repeat(indent));
            print_ast_tree(expr, indent + 2);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = args.get(1).expect("No input file specified");
    let print_ast = args.get(2).map_or(false, |arg| arg == "--ast");

    let input = fs::read_to_string(filename).expect("Failed to read input file");

    let result = run(&input, print_ast);
    println!(">>> {}", result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_run_with_file() {
        let code = r#"
            x = 3 + 5 * 2 
            echo x - 4
        "#;

        // Create a temporary file
        let mut temp_file = NamedTempFile::new().expect("Failed to create temp file");
        writeln!(temp_file, "{}", code).expect("Failed to write to temp file");

        // Get the path to the temp file
        let temp_path = temp_file
            .path()
            .to_str()
            .expect("Failed to convert path to string");

        // Read the file contents
        let input = std::fs::read_to_string(temp_path).expect("Failed to read temp file");

        // Run the function
        let result = run(&input, false);
        assert_eq!(result, 13);
    }
}
