use std::{collections::HashMap, error::Error, fmt, str::FromStr};

pub mod parsers;
pub mod cli;

pub struct Feature<'a>(&'a str, &'a str);

#[derive(Debug)]
pub struct ParseUposError;

impl<'a> fmt::Display for ParseUposError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error while parsing UPOS.")
    }
}

impl Error for ParseUposError {}

#[derive(Debug, PartialEq, Eq)]
pub enum UPOS {
    ADJ,
    ADP,
    ADV,
    AUX,
    CCONJ,
    DET,
    INTJ,
    NOUN,
    NUM,
    PART,
    PRON,
    PROPN,
    PUNCT,
    SCONJ,
    SYM,
    VERB,
    X,
}

impl FromStr for UPOS {
    type Err = ParseUposError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        use UPOS::*;
        match value {
            "ADJ" => Ok(ADJ),
            "ADP" => Ok(ADP),
            "ADV" => Ok(ADV),
            "AUX" => Ok(AUX),
            "CCONJ" => Ok(CCONJ),
            "DET" => Ok(DET),
            "INTJ" => Ok(INTJ),
            "NOUN" => Ok(NOUN),
            "NUM" => Ok(NUM),
            "PART" => Ok(PART),
            "PRON" => Ok(PRON),
            "PROPN" => Ok(PROPN),
            "PUNCT" => Ok(PUNCT),
            "SCONJ" => Ok(SCONJ),
            "SYM" => Ok(SYM),
            "VERB" => Ok(VERB),
            "X" => Ok(X),
            _ => Err(ParseUposError),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TokenID {
    Single(usize),
    Range(usize, usize),
}

type Features = HashMap<String, String>;

#[derive(Debug)]
pub struct Token {
    id: TokenID,
    form: String,
    lemma: Option<String>,
    upos: Option<UPOS>,
    features: Option<Features>,
    head: Option<TokenID>,
    deprel: Option<String>,
}

#[derive(Debug)]
pub struct Sentence {
    tokens: Vec<Token>,
}
