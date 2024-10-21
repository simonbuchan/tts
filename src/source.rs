use std::fmt;

#[derive(Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct SourcePos(pub usize);

impl fmt::Debug for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Half-open range of source positions: describes a span of text in a [`Source`].
/// [`std::ops::Range<SourcePos>`] has worse ergonomics: in particular it's not [`Copy`]!
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SourceSpan {
    pub start: SourcePos,
    pub end: SourcePos,
}

impl SourceSpan {
    pub fn new(start: SourcePos, end: SourcePos) -> Self {
        Self { start, end }
    }

    pub fn empty(pos: SourcePos) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    pub fn contains(&self, pos: SourcePos) -> bool {
        self.start <= pos && pos <= self.end
    }
}

/// 1-based line and column in a source file
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

pub struct Source {
    pub name: String,
    pub text: String,
}

impl Source {
    /// Create a Source object for a string
    pub fn new(name: String, text: String) -> Self {
        Self { name, text }
    }

    pub fn end(&self) -> SourcePos {
        SourcePos(self.text.len())
    }

    pub fn peek(&self, pos: SourcePos) -> Option<char> {
        self.text[pos.0..].chars().next()
    }

    pub fn scan_forward(
        &self,
        pos: SourcePos,
        mut while_char: impl FnMut(char) -> bool,
    ) -> SourcePos {
        match self.text[pos.0..].find(|c: char| !while_char(c)) {
            Some(len) => SourcePos(pos.0 + len),
            None => self.end(),
        }
    }

    /// Lookup line and column from this in a source file
    pub fn line_col(&self, pos: SourcePos) -> LineColumn {
        let mut line_start = 0;
        let mut line = 1;
        while let Some(line_len) = self.text[line_start..].find('\n') {
            let line_end = line_start + line_len + 1;
            if pos.0 < line_end {
                break;
            }
            line_start = line_end;
            line += 1;
        }
        let column = 1 + pos.0 - line_start;
        LineColumn { line, column }
    }

    /// Return the span for the line containing a position
    pub fn line_span(&self, pos: SourcePos) -> SourceSpan {
        let start = SourcePos(match self.text[..pos.0].rfind('\n') {
            Some(pos) => pos + 1,
            None => 0,
        });
        let end = SourcePos(match self.text[pos.0..].find('\n') {
            Some(len) => pos.0 + len,
            None => self.text.len(),
        });
        SourceSpan { start, end }
    }

    /// Return the text of the line containing a position
    pub fn line_text(&self, pos: SourcePos) -> &str {
        let span = self.line_span(pos);
        &&self[span]
    }
}

impl std::ops::Index<SourceSpan> for Source {
    type Output = str;

    fn index(&self, value: SourceSpan) -> &Self::Output {
        &self.text[value.start.0..value.end.0]
    }
}
