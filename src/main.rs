fn main() {
    let file = std::env::args().nth(1).unwrap();
    let mut lexer = keta::lexer::Lexer::new(&file);
    lexer.lex();
    let mut parser = keta::parser::Parser::new(&lexer);
    let ast_builder = parser.parse();
    let mut program = ast_builder.program;
    program.body.iter().for_each(|node| {
        println!("{:?}", node);
    });
    parser.semantic_analysis(&mut program).unwrap();
}
