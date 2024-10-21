//! Classify contiguous spans of source text into tokens to simplify parsing.

use crate::source::{Source, SourcePos, SourceSpan};

/// Kind of the token matched by the [`Lexer`]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    /// `"function"`
    KeywordFunction,
    /// `"var"`
    KeywordVar,
    /// `"type"`
    KeywordType,
    /// `"return"`
    KeywordReturn,

    /// `[0-9]+`
    NumericLiteral,
    /// `'"' [^"]* '"'`
    StringLiteral,
    /// `[_A-Za-z][_0-9A-Za-z]*`
    Identifier,

    /// `'='`
    Equals,
    /// `';'`
    Semicolon,
    /// `','`
    Comma,
    /// `','`
    Colon,
    /// `"=>"`
    Arrow,
    /// `'{'`
    OpenBrace,
    /// `'}'`
    CloseBrace,
    /// `'('`
    OpenParen,
    /// `')'`
    CloseParen,
    /// `'<'`
    LessThan,
    /// `'>'`
    GreaterThan,

    /// Unclosed string literal
    ErrorUnclosedString,
    /// Anything else
    ErrorUnknownChar,

    /// Lexer has not been initialized
    Start,
    /// Lexer has reach the end of input
    End,
}

/// Return a [`Lexer`] for the given source text.
/// Should be `Lexer::new()`, but this matches the original typescript code.
pub fn lex(source: &Source) -> Lexer<'_> {
    Lexer::new(source, SourcePos(0))
}

/// State of a lexer
pub struct Lexer<'source> {
    /// Input source
    pub source: &'source Source,
    /// The last recognized token kind.
    pub kind: TokenKind,
    /// The last recognized token source span.
    pub span: SourceSpan,
    /// The span end of the previous token.
    /// Handy to get full syntax node span.
    pub previous_end: SourcePos,
}

impl<'source> Lexer<'source> {
    /// Create a lexer that starts at the given position.
    /// Could be used later to re-scan a token to avoid storing length?
    pub fn new(source: &'source Source, pos: SourcePos) -> Self {
        let mut result = Self {
            source,
            kind: TokenKind::Start,
            span: SourceSpan::empty(pos),
            previous_end: pos,
        };
        result.scan();
        result
    }

    pub fn scan(&mut self) {
        assert_ne!(self.kind, TokenKind::End, "scan past end of input");
        self.previous_end = self.span.end;
        self.scan_forward(|c| matches!(c, ' ' | '\t' | '\r' | '\n'));
        self.span.start = self.span.end;
        self.kind = match self.peek() {
            None => TokenKind::End,
            Some('"') => {
                self.span.end.0 += 1;
                self.scan_forward(|c| c != '"');
                if self.peek() != Some('"') {
                    TokenKind::ErrorUnclosedString
                } else {
                    self.span.end.0 += 1;
                    TokenKind::StringLiteral
                }
            }
            Some('0'..='9') => {
                self.span.end.0 += 1;
                self.scan_forward(|c| matches!(c, '0'..='9'));
                TokenKind::NumericLiteral
            }
            Some('_' | 'A'..='Z' | 'a'..='z') => {
                self.span.end.0 += 1;
                self.scan_forward(|c| matches!(c, '_' | '0'..='9' | 'A'..='Z' | 'a'..='z'));
                let self1 = &self.source;
                let span = self.span;
                match &self1[span] {
                    "function" => TokenKind::KeywordFunction,
                    "var" => TokenKind::KeywordVar,
                    "type" => TokenKind::KeywordType,
                    "return" => TokenKind::KeywordReturn,
                    _ => TokenKind::Identifier,
                }
            }
            Some(c) => {
                self.span.end.0 += c.len_utf8();
                match c {
                    '=' => {
                        if self.peek() == Some('>') {
                            self.span.end.0 += 1;
                            TokenKind::Arrow
                        } else {
                            TokenKind::Equals
                        }
                    }
                    ',' => TokenKind::Comma,
                    ';' => TokenKind::Semicolon,
                    ':' => TokenKind::Colon,
                    '{' => TokenKind::OpenBrace,
                    '}' => TokenKind::CloseBrace,
                    '(' => TokenKind::OpenParen,
                    ')' => TokenKind::CloseParen,
                    '<' => TokenKind::LessThan,
                    '>' => TokenKind::GreaterThan,
                    _ => TokenKind::ErrorUnknownChar,
                }
            }
        }
    }

    /// Return the character (if any) after the current span end
    fn peek(&self) -> Option<char> {
        self.source.peek(self.span.end)
    }

    /// Advance the span end to the first position that does not match the
    /// provided predicate.
    fn scan_forward(&mut self, while_char: impl Fn(char) -> bool) {
        self.span.end = self.source.scan_forward(self.span.end, while_char);
    }
}

#[cfg(test)]
fn test(source: &str, expected: &[(TokenKind, &str)]) {
    let source = Source::new("test".into(), source.into());
    let mut lexer = lex(&source);
    let mut actual = vec![];
    let mut last_pos = SourcePos(0);
    while lexer.kind != TokenKind::End {
        assert_ne!(last_pos, lexer.span.end, "no progress");
        actual.push((lexer.kind, &source[lexer.span]));
        last_pos = lexer.span.end;
        lexer.scan();
    }
    assert_eq!(actual, expected);
}

#[test]
fn empty() {
    test("", &[]);
}

#[test]
fn basic() {
    test("x", &[(TokenKind::Identifier, "x")]);
}

#[test]
fn first() {
    test(
        " 1200Hello    World1! 14d",
        &[
            (TokenKind::NumericLiteral, "1200"),
            (TokenKind::Identifier, "Hello"),
            (TokenKind::Identifier, "World1"),
            (TokenKind::ErrorUnknownChar, "!"),
            (TokenKind::NumericLiteral, "14"),
            (TokenKind::Identifier, "d"),
        ],
    )
}

#[test]
fn underscore() {
    test(
        "x_y is _aSingle Identifier_",
        &[
            (TokenKind::Identifier, "x_y"),
            (TokenKind::Identifier, "is"),
            (TokenKind::Identifier, "_aSingle"),
            (TokenKind::Identifier, "Identifier_"),
        ],
    )
}

#[test]
fn var() {
    test(
        "var x = 1",
        &[
            (TokenKind::KeywordVar, "var"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Equals, "="),
            (TokenKind::NumericLiteral, "1"),
        ],
    )
}

#[test]
fn semicolon() {
    test(
        "x; y",
        &[
            (TokenKind::Identifier, "x"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Identifier, "y"),
        ],
    )
}

#[test]
fn newline() {
    test(
        "x\n y  \n",
        &[(TokenKind::Identifier, "x"), (TokenKind::Identifier, "y")],
    )
}

#[test]
fn string() {
    test(r#""hello""#, &[(TokenKind::StringLiteral, r#""hello""#)])
}

#[test]
fn brace() {
    test(
        "{ x: 1, y: \"string\" }",
        &[
            (TokenKind::OpenBrace, "{"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Colon, ":"),
            (TokenKind::NumericLiteral, "1"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "y"),
            (TokenKind::Colon, ":"),
            (TokenKind::StringLiteral, r#""string""#),
            (TokenKind::CloseBrace, "}"),
        ],
    )
}

#[test]
fn function() {
    test(
        "var f = function (x: number): number { return x }",
        &[
            (TokenKind::KeywordVar, "var"),
            (TokenKind::Identifier, "f"),
            (TokenKind::Equals, "="),
            (TokenKind::KeywordFunction, "function"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "x"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "number"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "number"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::KeywordReturn, "return"),
            (TokenKind::Identifier, "x"),
            (TokenKind::CloseBrace, "}"),
        ],
    )
}
