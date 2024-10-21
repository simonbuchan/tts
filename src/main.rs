#![deny(rust_2018_idioms)]

use tts::source::Source;

fn main() {
    for arg in std::env::args() {
        let source = std::fs::read_to_string(&arg).unwrap();
        let source = Source::new(arg, source);
        tts::compile(&source);
    }
}
