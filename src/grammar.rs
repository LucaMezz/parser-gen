use std::{collections::{HashMap, HashSet}, fmt};

use derive_more::derive::Constructor;
use itertools::Itertools;

/// Represents a LL(1) parse table for a Context Free Grammar.
/// 
/// The parse table is used to make derivations of strings which are in the language
/// defined by the CFG. It is used to verify whether or not a given string exists
/// in the language.
#[derive(Constructor, Clone, Debug)]
pub struct LLParseTable(HashMap<String, HashMap<String, Rule>>);

/// Simply displays an LL(1) parse table by describing, for each non-terminal row
/// and each terminal column, which production rule should be applied.
impl fmt::Display for LLParseTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (outer_key, inner_map) in &self.0 {
            writeln!(f, "{}:", outer_key)?;
            for (inner_key, rule) in inner_map {
                writeln!(f, "  {}: {}", inner_key, rule)?;
            }
        }
        Ok(())
    }
}

/// Represents a prediction for what production `rule` should be applied when the
/// next token in the input sequence is `terminal`.
#[derive(Constructor, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Predict {
    rule: Rule,
    terminal: String,
}

/// Represents a Context-Free Grammar.
/// 
/// `start` indicates what non-terminal that derivations of strings with this
/// grammar should start with, or be pushed onto the stack first when parsing.
/// 
/// `rules` is a list of the production rules in the grammar which define how
/// non-terminals in the grammar can be replaced to derive strings.
///  
#[derive(Constructor, Clone, Debug)]
pub struct Grammar {
    start: String,
    rules: Vec<Rule>
}

fn transform_recursive_rule(rule: &Rule) -> Rule {
    Rule {
        non_terminal: rule.non_terminal.clone() + "'",
        production: rule.production
            .iter()
            .skip(1)
            .cloned()
            .chain(std::iter::once(rule.non_terminal.clone() + "'"))
            .collect()
    }
}

fn transform_non_recursive_rule(rule: &Rule) -> Rule {
    Rule {
        non_terminal: rule.non_terminal.clone(),
        production: rule.production
            .iter()
            .cloned()
            .chain(std::iter::once(rule.non_terminal.clone() + "'"))
            .collect()
    }
}

/// Simply displays the grammar by displaying each of its production rules
/// each appearing on a new line.
impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.rules
            .iter()
            .for_each(|rule| writeln!(f, "{}", rule).unwrap());
        Ok(())
    }
}

impl Grammar {
    /// Eliminates left-recursive production rules within the grammar.
    /// 
    /// LL(k) parsing does not work if the grammar contains production rules
    /// which are left-recursive. This is because the parser could continue to apply
    /// the production rule indefinitely, as a result of the left-most-derivation
    /// which LL(k) uses.
    /// 
    /// Given some production rule:
    /// 
    ///     A -> Aα | β
    /// 
    /// Where α and β are some string of non-terminals. We can factor the left-recursion
    /// by introducing a new non-terminal A' and creating the following production rules
    /// 
    ///     A  -> βA'
    ///     A' -> αA'
    ///     A' -> ε
    /// 
    /// Which are equivalent, but eliminate the left-recursion.
    /// 
    pub fn left_factor(&self) -> Self {
        let unchanged_rules: Vec<Rule> = self.rules
            .iter()
            .filter(|&rule| !rule.left_recursive())
            .cloned()
            .collect();

        let factor: Vec<String> = self.rules
            .iter()
            .filter(|&rule| rule.left_recursive())
            .map(|rule| rule.non_terminal.clone())
            .unique()
            .collect();

        let additional_rules: Vec<Rule> = self.rules
            .iter()
            .filter(|&rule| factor.contains(&rule.non_terminal) && !rule.left_recursive())
            .map(|rule| transform_non_recursive_rule(rule))
            .collect();
        
        let empty_rules: Vec<Rule> = factor
            .iter()
            .map(|nt| Rule::empty(nt.to_string()))
            .collect();

        let factored_rules: Vec<Rule> = self.rules
            .iter()
            .filter(|&rule| rule.left_recursive())
            .map(|rule| 
                transform_recursive_rule(rule)
            )
            .collect();
        
        return Grammar::new(
            self.start.clone(), 
            vec![unchanged_rules, additional_rules, factored_rules, empty_rules].concat()
        );
    }

    /// Returns the set of all non-terminals which exist in the grammar.
    /// 
    /// The non-terminals are symbols in the grammar that can be broken down further
    /// into components. A non-terminal must appear on the left-hand-side of at least
    /// one production rule, but can appear freely on the right-hand-side of any
    /// production rules.
    pub fn non_terminals(&self) -> HashSet<String> {
        self.rules
            .iter()
            .map(|rule| rule.non_terminal.clone())
            .collect()
    }

    /// Returns the set of all terminals which exist in the grammar.
    /// 
    /// The terminals are symbols in the grammar that cannot be broken down further.
    /// Non-terminals never appear on the left-hand-side of a production rule in
    /// the grammar, but can appear freely on the right-hand-side.
    pub fn terminals(&self) -> HashSet<String> {
        self
            .alphabet()
            .difference(&self.non_terminals())
            .cloned()
            .collect()
    }

    /// Returns the `alphabet` of the grammar.
    /// 
    /// This returns all the symbols which appear in the grammar, including both
    /// terminals and non-terminals.
    pub fn alphabet(&self) -> HashSet<String> {
        self.rules
            .iter()
            .flat_map(|rule| {
                rule
                    .production
                    .iter()
                    .chain(std::iter::once(&rule.non_terminal))
            })
            .cloned()
            .collect()
    }

    pub fn predicts(&self) -> HashMap<String, HashSet<Predict>> {
        self.non_terminals()
            .iter()
            .map(|nt| (
                nt.clone(),
                self.rule_predicts(nt.clone())
            ))
            .collect()
    }

    fn rule_predicts(&self, non_terminal: String) -> HashSet<Predict> {
        let non_terminals = self.non_terminals();

        self.rules
            .iter()
            .filter(|&rule| rule.non_terminal == non_terminal)
            .flat_map(|rule| {
                let first = &rule.production[0];
                (non_terminals.contains(first))
                    .then(||self.rule_predicts(first.clone())
                        .iter()
                        .map(|x| Predict::new(rule.clone(), x.terminal.clone()))
                        .collect::<HashSet<_>>()
                    )
                    .unwrap_or_else(||
                        vec![Predict::new(rule.clone(), first.clone())]
                        .into_iter()
                        .collect())
            })
            .collect()
    }

    pub fn follows(&self) -> HashMap<String, HashSet<String>> {
        self.non_terminals()
            .iter()
            .map(|nt| (
                nt.clone(),
                self.follows_non_terminal(nt.clone())
            ))
            .collect()
    }

    pub fn follows_non_terminal(&self, non_terminal: String) -> HashSet<String> {
        let non_terminals = self.non_terminals();

        self.rules
            .iter()
            .flat_map(|rule| {
                rule.production
                    .iter()
                    .enumerate()
                    .filter(|(_index, &ref x)| x.clone() == non_terminal)
                    .flat_map(|(index, _)| {
                        let next = if index >= rule.production.len() - 1 {
                            rule.non_terminal.clone()
                        } else {
                            rule.production[index + 1].clone()
                        };
                        
                        (non_terminals.contains(&next))
                            .then(|| {
                                self.rule_predicts(next.clone())
                                    .iter()
                                    .map(|x| x.terminal.clone())
                                    .collect::<HashSet<_>>()
                            })
                            .unwrap_or_else(||
                                vec![next]
                                .into_iter()
                                .collect()
                        // !TODO
                        // Make sure not to forget the case here where for some state S,
                        // FOLLOWS(T) = FIRSTS(S) and FIRSTS(S) contains the empty string. 
                        // In this case, replace the empty string in the result with the
                        // things in FOLLOWS(S).
                        ) 
                    })
                    .collect::<HashSet<_>>()
            })
            .collect()
    }

    /// Constructs a LL(1) oarse table for the given grammar.
    /// 
    /// The parse table is 2-dimensional, and consists of rows which are precisely
    /// the terminals in the grammar, and columns which are precisely the non-terminals
    /// in the grammar. Each cell contains a production rule that should be applied if
    /// the parser ever finds itself in the given state.
    /// 
    /// For any given cell C in the table, if the top of the stack currently contains the
    /// non-terminal which is the row of C, and the current token of input is equal to
    /// the column of C, then the parser should pop the non-terminal, and push whatever
    /// appears on the right-hand-side of the production rule contained in cell C.
    /// 
    pub fn construct_ll_parse_table(&self) -> LLParseTable {
        // Make this a standalone function that takes a Grammar as input
        // and applies reductions on it first, then generates the parse table.
        let predicts = self.predicts();
        let follows = self.follows();

        LLParseTable(self.non_terminals()
            .iter()
            .map(|nt| {
                let predict = &predicts[nt];
                (nt.clone(),
                predict
                    .iter()
                    .map(|p| (p.terminal.clone(), p.rule.clone()))
                    .collect::<HashMap<_, _>>()
                )
            })
            .collect::<HashMap<_, _>>())
        
        // !TODO
        // Any time there is an empty string ε in the PREDICTS set of a non-terminal,
        // we take the FOLLOWS of that non-terminal row and use its terminals to
        // indicate which columns to use.
    }
}

/// A production rule in a Context Free Grammar.
/// 
/// Consists of the non-terminal which the production rule is for, followed by the
/// series of terminal and non-terminal symbols that make up the production rule.
/// 
/// An example of a production rule of arbitrary length:
/// 
///     S -> ABcXy ... z
/// 
#[derive(Constructor, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Rule {
    non_terminal: String,
    production: Vec<String>,
}

impl fmt::Display for Rule {
    /// Formats the production rule.
    /// 
    /// The non-terminal which the production rule is for is placed to the left,
    /// followed by an arrow `->`. After this, the series of terminal and non-terminal
    /// symbols that make up the production rule are displayed.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.non_terminal, self.production.join(" "))
    }
}

impl Rule {
    /// Determines if the rule is left-recursive. 
    /// 
    /// This is when the non-terminal
    /// on the left-hand-side of the production rule is also the first thing
    /// that appears on the right-hand side of the non-terminal.
    /// 
    /// In order to construct an LL(1) parser for a grammar containing this
    /// rule, the grammar would need to be modified to remove this recursion.
    pub fn left_recursive(&self) -> bool {
        self.non_terminal == self.production[0]
    }

    /// Creates a new production rule for the given terminal, where the
    /// result of the production is just the empty string ε.
    /// 
    /// This empty production rule can be applied without pushing anything
    /// new onto the stack. Therefore, the non-terminal is just consumed. 
    pub fn empty(non_terminal: String) -> Self {
        Rule::new(non_terminal, vec![String::from("ε")])
    }

    /// Creates a new production rule from a non-terminal, and a series
    /// of symbols in the production, which could be terminals or non-terminals.
    /// 
    /// This is a helper function to automatically convert string slices to
    /// Strings which we have ownership over.
    pub fn from<'a>(non_terminal: &'a str, production: Vec<&'a str>) -> Self {
        Rule::new(
            non_terminal.to_owned(), 
            production
                .iter()
                .map(|&s| s.to_string())
                .collect()
        )
    }
}