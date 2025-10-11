use ariadne::Span;
use internment::ArcIntern;
use std::cmp::{max, min};
use std::ops::Range;
use std::path::PathBuf;

pub trait Located {
    fn location(&self) -> Location;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Location {
    file: ArcIntern<PathBuf>,
    span: Range<usize>,
}

impl Span for Location {
    type SourceId = ArcIntern<PathBuf>;

    fn source(&self) -> &Self::SourceId {
        &self.file
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

impl Location {
    pub fn new(file: &ArcIntern<PathBuf>, span: Range<usize>) -> Self {
        Location {
            file: file.clone(),
            span,
        }
    }

    pub fn extend_to(&self, other: &Location) -> Location {
        assert_eq!(self.file, other.file);
        Location {
            file: self.file.clone(),
            span: min(self.span.start, other.span.start)..max(self.span.end, other.span.end),
        }
    }

    pub fn merge_span(mut self, span: Range<usize>) -> Location {
        self.span = min(self.span.start, span.start)..max(self.span.end, span.end);
        self
    }

    pub fn file(&self) -> &str {
        self.file.to_str().unwrap_or("<bad_name>")
    }

    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}
