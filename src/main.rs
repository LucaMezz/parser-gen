use codegen::{CodeGenConfig, ParserType, TargetLanguage};
use grammar::{Grammar, Rule};

mod tests;

mod grammar;
mod codegen;

fn main() {
    //let grammar = Grammar::from(vec![
    //    Rule::from("A"), vec!["A"), "b")]),
    //    Rule::from("A"), vec!["c")]),
    //]);

    let grammar: Grammar = Grammar::new(
        String::from("S"),
        vec![
            Rule::from("S", vec!["a"]),
            Rule::from("S", vec!["B"]),
            Rule::from("B", vec!["C", "d", "O"]),
            Rule::from("C", vec!["E", "g"]),
            Rule::from("C", vec!["f", "g"]),
            Rule::from("E", vec!["a"]),
            Rule::from("A", vec!["h"]),
            Rule::from("O", vec!["j"]),
            Rule::from("O", vec!["B"])
        ]
    );

    // let grammar: Grammar = Grammar::new(
    //     "S"),
    //     vec![
    //     Rule::from("S", vec!["i","E","t","S"]),
    //     Rule::from("S", vec!["i","E","t","S","e","S"]),
    //     Rule::from("S", vec!["a"]),
    //     Rule::from("E", vec!["b"])
    // ]);

    let factored_grammar = grammar.left_factor();

    println!("Factored grammar: \n{}", factored_grammar);

    println!("Firsts: \n{:#?}", factored_grammar.predicts());

    println!("Follows: \n{:#?}", factored_grammar.follows());

    println!("Parse Table: \n{}", factored_grammar.construct_ll_parse_table());

    let target_language = TargetLanguage::Rust;
    let parser_type = ParserType::LL;
    let config = CodeGenConfig::new(target_language, parser_type);
    let generated_code = config.generate(&grammar);

    println!("Generated code:\n\n{}", generated_code);
}
