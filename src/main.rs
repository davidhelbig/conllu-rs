use std::{path::PathBuf, fs::File, io::BufReader};

use clap::Parser;
use conllu_rs::{parsers::Doc, Sentence, cli};



fn main() {
    let cli = cli::Cli::parse();
    let file = File::open(cli.file).unwrap();

    let reader = BufReader::new(file);

    let doc = Doc::new(reader);

    let sentences: Result<Vec<Sentence>, String> = doc.into_iter().collect();

    println!("{len:?}", len = sentences.unwrap().len());
}
