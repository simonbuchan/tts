use crate::bind::Meaning;
use crate::check::TypeId;
use crate::source::{Source, SourceSpan};
use colorful::Color;
use std::fmt::{self, Write as _};

pub struct Reporter<'out> {
    reports: &'out mut Vec<Report>,
}

impl<'out> Reporter<'out> {
    pub fn new(reports: &'out mut Vec<Report>) -> Self {
        Self { reports }
    }

    pub fn note(&mut self, span: SourceSpan, message: Message) -> Reporter<'_> {
        self.report(ReportKind::Note, span, message)
    }

    pub fn info(&mut self, span: SourceSpan, message: Message) -> Reporter<'_> {
        self.report(ReportKind::Info, span, message)
    }

    pub fn warning(&mut self, span: SourceSpan, message: Message) -> Reporter<'_> {
        self.report(ReportKind::Warning, span, message)
    }

    pub fn error(&mut self, span: SourceSpan, message: Message) -> Reporter<'_> {
        self.report(ReportKind::Error, span, message)
    }

    pub fn report(&mut self, kind: ReportKind, span: SourceSpan, message: Message) -> Reporter<'_> {
        self.reports.push(Report {
            kind,
            span,
            message,
            also: vec![],
        });
        let last = self.reports.last_mut().expect("just pushed");
        Reporter::new(&mut last.also)
    }
}

#[derive(Clone, Debug)]
pub struct Report {
    pub kind: ReportKind,
    pub span: SourceSpan,
    pub message: Message,
    pub also: Vec<Report>,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum ReportKind {
    Note,
    Info,
    Warning,
    Error,
}

impl ReportKind {
    fn as_str(self) -> &'static str {
        match self {
            Self::Note => "note",
            Self::Info => "info",
            Self::Warning => "warning",
            Self::Error => "error",
        }
    }

    fn color(self) -> Color {
        match self {
            Self::Note => Color::Cyan,
            Self::Info => Color::LightGray,
            Self::Warning => Color::Yellow,
            Self::Error => Color::Red,
        }
    }
}

impl fmt::Display for ReportKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

#[derive(Clone, Debug)]
pub enum Message {
    // Lexer (forwarded by parser for TokenKind::Error*)
    /// `"unclosed string"`
    UnclosedString,

    /// `"unknown char"`
    UnknownChar,

    // Parser
    /// `"expected {expected}"`
    Expected { expected: &'static str },

    /// `"unclosed"`
    Unclosed,

    // Binder
    /// `"already declared: {name}"`
    Redeclared { name: String },
    /// - `"first declared here"`
    FirstDeclared,

    // Checker
    /// `"cannot assign {right_type} to value of type {left_type}"`
    Assign { left: TypeId, right: TypeId },
    /// `"cannot call expression of type {ty}"`
    Call { ty: TypeId },
    /// `"declared type {declared_type} does not match returned type {return_type}"`
    ReturnType {
        declared_type: TypeId,
        return_type: TypeId,
    },
    /// `"could not resolve {name} as {type]"`
    Resolve { name: String, meaning: Meaning },
    /// `"expected {parameter_count} arguments, but got {argument_count}"`
    ArgumentCount {
        parameter_count: usize,
        argument_count: usize,
    },
    /// `"expected argument of type {parameter_type}, but got {argument_type}"`
    Argument {
        parameter_type: TypeId,
        argument_type: TypeId,
    },
}

struct IndentWrite<W> {
    write: W,
    indent: usize,
    at_line_start: bool,
}

impl<W: fmt::Write> fmt::Write for IndentWrite<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for line in s.split_inclusive('\n') {
            if self.at_line_start {
                for _ in 0..self.indent {
                    self.write.write_char(' ')?;
                }
            }
            self.write.write_str(line)?;
            self.at_line_start = line.ends_with('\n');
        }
        Ok(())
    }
}

pub struct Context<'a> {
    source: &'a Source,
}

pub struct ReportFormatter<'a> {
    write: IndentWrite<&'a mut dyn fmt::Write>,
    context: Context<'a>,
}

impl<'a> ReportFormatter<'a> {
    pub fn new(write: &'a mut dyn fmt::Write, source: &'a Source) -> Self {
        Self {
            write: IndentWrite {
                write,
                indent: 0,
                at_line_start: true,
            },
            context: Context { source },
        }
    }
}

impl ReportFormatter<'_> {
    pub fn write(&mut self, report: &Report) -> fmt::Result {
        let start_loc = self.context.source.line_col(report.span.start);
        writeln!(
            self.write,
            "{filename}:{start_loc}: {kind}: {message}",
            filename = self.context.source.name,
            kind = report.kind,
            message = WithContext(&self.context, &report.message),
        )?;
        let start_line_span = self.context.source.line_span(report.span.start);
        writeln!(
            self.write,
            "{line:>4} | {source}",
            line = start_loc.line,
            source = &self.context.source[start_line_span]
        )?;
        // squiggle
        write!(self.write, "     | ")?;
        for _ in 1..start_loc.column {
            self.write.write_char(' ')?
        }
        if start_line_span.contains(report.span.end) {
            // single line
            for _ in report.span.start.0..report.span.end.0 {
                self.write.write_char('^')?
            }
            writeln!(self.write)?;
        } else {
            // multiple lines - only show first line and last line
            for _ in report.span.start.0..start_line_span.end.0 {
                self.write.write_char('^')?
            }
            writeln!(self.write)?;
            let end_loc = self.context.source.line_col(report.span.end);
            let end_line_span = self.context.source.line_span(report.span.end);
            writeln!(
                self.write,
                "{line:>4} | {source}",
                line = end_loc.line,
                source = &self.context.source[end_line_span]
            )?;
            write!(self.write, "     | ")?;
            for _ in end_line_span.start.0..report.span.end.0 {
                self.write.write_char('^')?
            }
            writeln!(self.write)?;
        }

        self.write.indent += 2;
        for report in &report.also {
            if let Err(error) = self.write(report) {
                self.write.indent -= 2;
                return Err(error);
            }
        }
        self.write.indent -= 2;

        Ok(())
    }
}

struct WithContext<'a, 'b, T>(&'b Context<'a>, T);

impl fmt::Display for WithContext<'_, '_, &Message> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.1 {
            // Lexer
            Message::UnknownChar => write!(f, "unknown char"),
            Message::UnclosedString => write!(f, "unclosed string"),

            // Parser
            Message::Expected { expected } => write!(f, "expected {expected}"),
            Message::Unclosed => write!(f, "unclosed"),

            // Binder
            Message::Redeclared { name } => write!(f, "already declared: {name}"),
            Message::FirstDeclared => write!(f, "first declared here"),

            // Checker
            Message::Assign { left, right } => {
                write!(f, "cannot assign {right:?} to value of type {left:?}")
            }
            Message::Call { ty } => {
                write!(f, "cannot call expression of type {ty:?}")
            }
            Message::ReturnType {
                declared_type,
                return_type,
            } => {
                // todo: resolve types
                write!(
                    f,
                    "declared type {declared_type:?} does not match returned type {return_type:?}"
                )
            }
            Message::Resolve { name, meaning } => {
                write!(f, "could not resolve {name:?} as {meaning:?}")
            }
            Message::ArgumentCount {
                parameter_count,
                argument_count,
            } => {
                write!(
                    f,
                    "expected {parameter_count} arguments, but got {argument_count}"
                )
            }
            Message::Argument {
                parameter_type,
                argument_type,
            } => {
                write!(
                    f,
                    "expected argument of type {parameter_type:?}, but got {argument_type:?}"
                )
            }
        }
    }
}
