use std::{collections::HashSet, fmt::Error};

use derive_more::derive::Constructor;
use genco::prelude::*;
use inflector::Inflector;
use itertools::Itertools;

use crate::{codegen::CodeGenerator, grammar::{Grammar, LLParseTable, Rule}};

#[derive(Constructor)]
pub struct RustLLGenerator;

impl CodeGenerator for RustLLGenerator {
    /// Generates the source code for an LL(1) parser from the grammar specified.
    fn generate_parser(&self, grammar: &Grammar) -> String {
        let reduced_grammar = grammar.left_factor();
        let parse_table = reduced_grammar.construct_ll_parse_table();

        let terminals = reduced_grammar.terminals()
            .into_iter()
            .map(|t| t.to_title_case())
            .collect();

        let non_terminals = reduced_grammar.non_terminals();

        let token_enum = generate_token_enum(&terminals);
        let non_terminal_enum = generate_non_terminal_enum(&non_terminals);
        let parse = generate_parse(&reduced_grammar, &parse_table);

        quote! {
            $(token_enum)

            $(non_terminal_enum)

            $(parse)
        }
            .to_file_string()
            .unwrap()
    }
}

/// Generates the Enumeration of all tokens / terminals in the grammar.
/// 
/// The parse function is used to parse a string and see if it matches the
/// grammar used to generate the parser.
fn generate_token_enum(terminals: &HashSet<String>) -> rust::Tokens {
    quote! {
        #[derive(PartialEq)]
        pub enum Token {
            $(for terminal in terminals join(, ) => $terminal)
        }
    }
}

/// Generates the Enumeration of all non-terminals that appear in the grammar.
fn generate_non_terminal_enum(non_terminals: &HashSet<String>) -> rust::Tokens {
    quote! {
        #[derive(PartialEq)]
        pub enum NonTerminal {
            $(for non_terminal in non_terminals join(, ) => $non_terminal)
        }
    }
}

/// Generates the main parser struct.
fn generate_parse(grammar: &Grammar, parse_table: &LLParseTable) -> rust::Tokens {
    let productions = generate_productions(&grammar.non_terminals(), parse_table);

    quote! {
        enum StackAlphabet {
            Token(Token),
            NonTerminal(NonTerminal),
        }

        pub struct Parser {
            stack: Vec<StackAlphabet>,
            current: usize,
            input: Vec<Token>,
        }

        impl Parser {
            pub fn new() -> Self {
                Self {
                    stack: vec![],
                    current: 0,
                    input: vec![],
                }
                
            }

            pub fn parse<'a>(&mut self, input: Vec<Token>) {
                self.input = input;
                self.current = 0;
                self.stack = vec![StackAlphabet::NonTerminal(NonTerminal::$(grammar.start()))]; // push the start symbol to the top of the stack.
        
                while self.current < self.input.len() {
                    self.process();
                }
            }
        
            fn process(&mut self) {
                while let Some(top) = self.stack.last() {
                    use StackAlphabet::*;
                    match top {
                        Token(token) => {
                            if *token == self.input[self.current] {
                                self.stack.pop();
                                self.current += 1;
                            } else {
                                panic!();
                            }
                        }
                        NonTerminal(_) => break
                    }
                }

                if self.input_complete() {
                    return;
                }
        
                let next = self.next();
                let top = self.stack.last().unwrap();

                $(productions)

                // generate a massive match statement here based on the parse table
                // which applies the respective production rule / produces errors
                // where required based on input and stack.
            }
        
            fn next(&self) -> &Token {
                &self.input[self.current]
            }

            fn input_complete(&self) -> bool {
                self.current >= self.input.len()
            }
        }
    }
}

/// Generates the match statement which performs the required production rule
/// based on what symbol is on top of the stack, and what is being read from
/// the input token stream.
fn generate_productions(non_terminals: &HashSet<String>, parse_table: &LLParseTable) -> rust::Tokens {
    quote! {
        match top {
            StackAlphabet::NonTerminal(nt) => match nt {
                $(for non_terminal in non_terminals join($['\n']) => 
                    NonTerminal::$non_terminal => $(quote! {
                        match next {
                            $(for terminal in parse_table
                                .table()
                                .get(non_terminal)
                                .unwrap()
                                .keys() 
                            join(, ) => 
                                $(generate_production(
                                    terminal.to_title_case(), 
                                    parse_table
                                        .table()
                                        .get(non_terminal)
                                        .unwrap()
                                        .get(terminal)
                                        .unwrap(),
                                    non_terminals,
                                ))
                            ),
                            _ => panic!()
                        }
                    }),)
            }
            StackAlphabet::Token(_) => panic!()
        }
    }
}

/// Generates the push and pop instruction for the stack which should happen for a 
/// given rule when the non_terminal on the LHS of that rule is found on the top of
/// the stack and the given terminal is the next token in the input.
fn generate_production(terminal: String, rule: &Rule, non_terminals: &HashSet<String>) -> rust::Tokens {
    quote! {
        Token::$(terminal) => {
            self.stack.pop();

            $(for symbol in rule
                .production()
                .into_iter()
                .rev()
                .collect::<Vec<_>>() join(;$['\n']) => 
                    self.stack.push($(if non_terminals.contains(&symbol) {
                        StackAlphabet::NonTerminal(NonTerminal::$symbol)
                    } else { 
                        StackAlphabet::Token(Token::$(symbol.to_title_case()))
                    }
                ))
            )
        }
    }
}