use tracing::{info, trace};

fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::INFO)
        .with_ansi(true)
        .with_level(false)
        .without_time()
        .with_line_number(true)
        .pretty()
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();
    let file = std::env::args().nth(1).unwrap();
    let mut lexer = keta::lexer::Lexer::new(&file);
    lexer.lex();
    let mut parser = keta::parser::Parser::new(&lexer);
    let mut ast_builder = parser.parse();
    let program = ast_builder.program;
    for node in &program.body {
        info!("{}", node.node_type);
    }
    parser
        .semantic_analysis(&program, &mut ast_builder.context)
        .unwrap();
}
