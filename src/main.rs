fn main() {
    let file = std::env::args().nth(1).unwrap();
    let mut lexer = keta::lexer::Lexer::new(&file);
    lexer.lex();
    for token in lexer.tokens.iter() {
        println!("{}", token);
    }
    let mut parser = keta::parser::Parser::new(&lexer);
    let ast_builder = parser.parse();
    let program = ast_builder.program;
    program.body.iter().for_each(|node| {
        println!("{:?}", node);
    });
}
