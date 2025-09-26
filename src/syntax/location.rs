use codespan_reporting::diagnostic::Label;
use std::cmp::{max, min};
use std::ops::Range;

pub trait Located {
    fn location(&self) -> Location;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Location {
    file_id: usize,
    span: Range<usize>,
}

impl Location {
    pub fn new(file_id: usize, span: Range<usize>) -> Self {
        Location { file_id, span }
    }

    pub fn extend_to(&self, other: &Location) -> Location {
        assert_eq!(self.file_id, other.file_id);
        Location {
            file_id: self.file_id,
            span: min(self.span.start, other.span.start)..max(self.span.end, other.span.end),
        }
    }

    pub fn merge_span(mut self, span: Range<usize>) -> Location {
        self.span = min(self.span.start, span.start)..max(self.span.end, span.end);
        self
    }

    pub fn file_id(&self) -> usize {
        self.file_id
    }

    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn primary_label(&self) -> Label<usize> {
        Label::primary(self.file_id, self.span.clone())
    }

    pub fn secondary_label(&self) -> Label<usize> {
        Label::secondary(self.file_id, self.span.clone())
    }
}
