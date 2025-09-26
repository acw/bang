use crate::syntax::{Located, Location};
use std::cmp;
use std::fmt;
use std::hash;
use std::sync::atomic::{AtomicU64, Ordering};

static IDENTIFIER_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug)]
pub struct Name {
    printable: String,
    identifier: u64,
    location: Option<Location>,
}

impl cmp::PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.identifier == other.identifier
    }
}

impl cmp::Eq for Name {}

impl hash::Hash for Name {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.identifier.hash(state);
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.printable, self.identifier)
    }
}

impl Name {
    pub fn new<S: ToString>(location: Location, s: S) -> Name {
        let my_id = IDENTIFIER_COUNTER.fetch_add(1, Ordering::SeqCst);
        Name {
            printable: s.to_string(),
            identifier: my_id,
            location: Some(location),
        }
    }

    pub fn gensym(base: &'static str) -> Name {
        let formatted = format!("<{base}>");
        let my_id = IDENTIFIER_COUNTER.fetch_add(1, Ordering::SeqCst);

        Name {
            printable: formatted,
            identifier: my_id,
            location: None,
        }
    }

    pub fn as_printed(&self) -> &str {
        self.printable.as_str()
    }
}
