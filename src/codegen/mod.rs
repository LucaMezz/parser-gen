use derive_more::derive::Constructor;
use target::rust::RustLLGenerator;

use crate::grammar::Grammar;

pub mod target;

pub enum TargetLanguage {
    Rust,
}

pub enum ParserType {
    LL
}

trait CodeGenerator {
    fn generate_parser(&self, grammar: &Grammar) -> String;
}

#[derive(Constructor)]
pub struct CodeGenConfig {
    target_language: TargetLanguage,
    parser_type: ParserType
}

impl CodeGenConfig {
    pub fn generate(&self, grammar: &Grammar) -> String {
        use TargetLanguage::*;
        use ParserType::*;
        match self.target_language {
            Rust => match self.parser_type {
                LL => RustLLGenerator::new().generate_parser(grammar), },
        }
    }
}