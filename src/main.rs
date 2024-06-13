fn main() {
    let file = std::env::args().nth(1).unwrap();
    let mut lexer = keta::lexer::Lexer::new(&file);
    lexer.lex();
    for token in lexer.tokens.iter() {
        println!("{}", token);
    }
}