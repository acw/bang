use std::collections::{HashMap, hash_map};

/// The directional associativity for an operator.
///
/// This directionality impacts whether (a + b + c) defaults to
/// ((a + b) + c) or (a + (b + c)). It does not effect situations
/// in which operator numeric precedence is different between
/// operators.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Associativity {
    Left,
    Right,
    None,
}

/// The kind of operators we use. This is only narrowly useful inside
/// this particular crate.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OperatorType {
    Prefix,
    Infix,
    Postfix,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OperatorClass {
    Value,
    Type,
}

#[derive(Default)]
pub struct PrecedenceTable {
    table: HashMap<String, Vec<(OperatorClass, OperatorType, Associativity, u8)>>,
}

impl PrecedenceTable {
    pub fn add_operator(
        &mut self,
        opclass: OperatorClass,
        optype: OperatorType,
        assoc: Associativity,
        name: String,
        precedence: u8,
    ) {
        match self.table.entry(name) {
            hash_map::Entry::Vacant(vac) => {
                vac.insert(vec![(opclass, optype, assoc, precedence)]);
            }

            hash_map::Entry::Occupied(mut occ) => {
                for (excl, exty, exass, existing) in occ.get_mut().iter_mut() {
                    if *excl == opclass && *exty == optype {
                        *exass = assoc;
                        *existing = precedence;
                        return;
                    }
                }

                occ.get_mut().push((opclass, optype, assoc, precedence));
            }
        }
    }

    pub fn get_precedence(
        &self,
        opclass: OperatorClass,
        optype: OperatorType,
        name: &String,
    ) -> Option<(u8, u8)> {
        let items = self.table.get(name)?;

        for (cl, ty, assoc, value) in items.iter() {
            if &opclass == cl && &optype == ty {
                let even = *value * 2;
                let high = even + 1;

                match assoc {
                    Associativity::Left => return Some((even, high)),
                    Associativity::Right => return Some((high, even)),
                    Associativity::None => return Some((even, even)),
                }
            }
        }

        None
    }
}
