#![deny(rust_2018_idioms)]

mod list;
use crate::source::Source;
pub use bind::bind;
pub use check::check;
pub use lex::lex;
pub use parse::parse;

pub mod bind;
pub mod check;
pub mod diag;
pub mod lex;
pub mod parse;
pub mod source;
pub mod syntax;

pub struct CompileResult {
    pub syntax: syntax::SyntaxList,
    pub module: syntax::Module,
    pub bindings: bind::BindResult,
    pub check: check::CheckResult,
    pub reports: Vec<diag::Report>,
}

pub fn compile(source: &Source) -> CompileResult {
    let mut reports = vec![];
    let mut reporter = diag::Reporter::new(&mut reports);
    let parsed = parse(lex(&source), &mut reporter);
    let bindings = bind(&parsed, &mut reporter);
    let check = check(&parsed, &bindings, &mut reporter);
    // no emit(transform(tree))
    CompileResult {
        module: parsed.module,
        syntax: parsed.syntax,
        bindings,
        check,
        reports,
    }
}

#[cfg(test)]
mod tests;
