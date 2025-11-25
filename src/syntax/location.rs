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

    pub fn manufactured() -> Self {
        Location {
            file: ArcIntern::new("<manufactured>".into()),
            span: 0..0,
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
}

#[test]
fn extension_and_merge() {
    let file = ArcIntern::new("/foo/bar.txt".into());
    let loc1 = Location::new(&file, 1..4);
    let loc2 = Location::new(&file, 4..8);

    assert_eq!(loc1.extend_to(&loc2).source(), &file);
    assert_eq!(loc1.extend_to(&loc2).start(), 1);
    assert_eq!(loc1.extend_to(&loc2).end(), 8);

    let loc3 = Location::new(&file, 12..16);
    assert_eq!(loc1.extend_to(&loc3).source(), &file);
    assert_eq!(loc1.extend_to(&loc3).start(), 1);
    assert_eq!(loc1.extend_to(&loc3).end(), 16);

    assert_eq!(loc1.clone().merge_span(0..1).start(), 0);
    assert_eq!(loc1.merge_span(0..1).end(), 4);
}
