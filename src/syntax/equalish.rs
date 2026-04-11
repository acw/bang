use crate::syntax::ast::*;
use crate::syntax::name::Name;

pub trait Equalish {
    fn equalish(&self, other: &Self) -> bool;
}

impl Equalish for Name {
    fn equalish(&self, other: &Self) -> bool {
        self.as_printed() == other.as_printed()
    }
}

impl Equalish for Type {
    fn equalish(&self, other: &Self) -> bool {
        if let Type::Application(app, args) = other
            && args.is_empty()
            && self.equalish(app)
        {
            return true;
        }

        match self {
            Type::Primitive(_, name1) => {
                matches!(other, Type::Primitive(_, name2) if name1.equalish(name2))
            }
            Type::Variable(_, name1) => {
                matches!(other, Type::Variable(_, name2) if name1.equalish(name2))
            }
            Type::Constructor(_, name1) => {
                matches!(other, Type::Constructor(_, name2) if name1.equalish(name2))
            }
            Type::Function(args1, ret1) => matches!(other, Type::Function(args2, ret2) if
                    args1.equalish(args2) && ret1.equalish(ret2)),
            Type::Application(app1, args1) => {
                matches!(other, Type::Application(app2, args2) if
                    app1.equalish(app2) && args1.equalish(args2))
                    || (args1.is_empty() && app1.equalish(other))
            }
        }
    }
}

impl<T: Equalish> Equalish for Vec<T> {
    fn equalish(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for (left, right) in self.iter().zip(other.iter()) {
            if !left.equalish(right) {
                return false;
            }
        }

        true
    }
}
