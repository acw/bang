use crate::syntax::Location;
#[cfg(test)]
use internment::ArcIntern;
use std::cmp;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};

static IDENTIFIER_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug)]
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

impl Hash for Name {
    fn hash<H: Hasher>(&self, state: &mut H) {
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

    pub fn bind_to(&mut self, other: &Name) {
        self.identifier = other.identifier;
    }

    pub fn location(&self) -> Option<&Location> {
        self.location.as_ref()
    }
}

#[test]
fn equality() {
    let file = ArcIntern::new("/foo.bang".into());
    let loc1 = Location::new(&file, 0..3);
    let loc2 = Location::new(&file, 9..12);

    assert_ne!(Name::gensym("x"), Name::gensym("x"));
    assert_ne!(Name::new(loc1.clone(), "x"), Name::new(loc1.clone(), "x"));
    assert_eq!(
        Name {
            printable: "x".into(),
            identifier: 5,
            location: Some(loc1.clone())
        },
        Name {
            printable: "x".into(),
            identifier: 5,
            location: Some(loc2.clone())
        }
    );
    assert_eq!(
        Name {
            printable: "x".into(),
            identifier: 5,
            location: Some(loc1.clone())
        },
        Name {
            printable: "x".into(),
            identifier: 5,
            location: None
        }
    );
    assert_eq!(
        Name {
            printable: "x".into(),
            identifier: 5,
            location: Some(loc1.clone())
        },
        Name {
            printable: "y".into(),
            identifier: 5,
            location: None
        }
    );
}

#[test]
fn hashing() {
    let file = ArcIntern::new("/foo.bang".into());
    let loc1 = Location::new(&file, 0..3);
    let loc2 = Location::new(&file, 9..12);

    let x1 = Name {
        printable: "x".into(),
        identifier: 1,
        location: Some(loc1),
    };
    let mut x2 = Name {
        printable: "x".into(),
        identifier: 2,
        location: Some(loc2),
    };
    let y1 = Name {
        printable: "y".into(),
        identifier: 1,
        location: None,
    };

    let run_hash = |name: &Name| {
        let mut hash = std::hash::DefaultHasher::new();
        name.hash(&mut hash);
        hash.finish()
    };

    let hash_x1 = run_hash(&x1);
    let hash_x2 = run_hash(&x2);
    let hash_y1 = run_hash(&y1);

    assert_ne!(hash_x1, hash_x2);
    assert_eq!(hash_x1, hash_y1);

    x2.bind_to(&x1);
    let rehashed_x2 = run_hash(&x2);
    assert_eq!(hash_x1, rehashed_x2);
}
