use std::fmt::Arguments;

use crate::syntax::ast::{ConstantValue, IntegerWithBase, Type};
use crate::syntax::location::Location;
use crate::syntax::name::Name;
use itertools::Itertools;
use proptest::arbitrary::Arbitrary;
use proptest::prelude::{BoxedStrategy, Rng};
use proptest::prop_oneof;
use proptest::strategy::{NewTree, Strategy, ValueTree};
use proptest::test_runner::TestRunner;

const MAXIMUM_TYPE_DEPTH: usize = 5;
const MAXIMUM_TYPE_WIDTH: usize = 5;
const MAXIMUM_STRING_SIZE: usize = 32;
const PRIMITIVE_TYPES: &[&str] = &[
    "Char", "String", "I8", "I16", "I32", "I64", "U8", "U16", "U32", "U64",
];

#[derive(Debug, Default)]
pub struct TypeGenerationContext {
    available_constructors: Vec<Name>,
    available_variables: Vec<Name>,
}

impl TypeGenerationContext {
    fn generate_type(&mut self, runner: &mut TestRunner, depth: usize) -> Type {
        let mut leaf_options = vec![];

        if !self.available_constructors.is_empty() {
            for name in self.available_constructors.iter() {
                leaf_options.push(Type::Constructor(
                    Location::manufactured(),
                    name.clone(),
                ));
            }
        }

        if !self.available_variables.is_empty() {
            for name in self.available_variables.iter() {
                leaf_options.push(Type::Variable(
                    Location::manufactured(),
                    name.clone(),
                ));
            }
        }

        for prim in PRIMITIVE_TYPES.iter() {
            leaf_options.push(Type::Primitive(
               Location::manufactured(),
               Name::new(Location::manufactured(), prim.to_string()),
            ));
        }

        if depth < MAXIMUM_TYPE_DEPTH && runner.rng().random_bool(0.5) {
        }

        let index = runner.rng().random_range(0..leaf_options.len());
        leaf_options.remove(index)
    }
}

#[derive(Clone)]
pub struct TypeGenerationTree {
    current_value: Type,
    parent: Option<Box<TypeGenerationTree>>,
    untried_simplified_items: Option<Vec<TypeGenerationTree>>,
}

impl TypeGenerationTree {
    /// Create a new type generation tree based on the given
    /// initial value.
    pub fn new(initial_value: Type) -> TypeGenerationTree {
        TypeGenerationTree {
            current_value: initial_value,
            parent: None,
            untried_simplified_items: None,
        }
    }
}

fn generate_powerset(_: &[Type]) -> Vec<Vec<Type>> {
    vec![]
}

fn simplify_type(incoming: &Type) -> Vec<Type> {
    match incoming {
        Type::Primitive(_, _) => vec![],
        Type::Constructor(_, _) => vec![],
        Type::Variable(_, _) => vec![],
        Type::Function(arg_types, ret_type) => {
            let simplified_return_types = simplify_type(ret_type.as_ref());

            // we do the following as a set of steps, choosing to go deep rather than
            // broad immediately. So this works as follows:
            //
            //  1. If there are simplifications for the return type, then just
            //     return variations with the simplified return type.
            //  2. If there are simplifications for the first argument, then
            //     just return variations with the first argument simplified.
            //  3. Repeat for each of the arguments.
            //  4. At this point, all the subtypes are as simple as they can
            //     be, so return a series of function types with fewer arguments.
            //  5. If we are a function with no arguments, then just return
            //     the return type.
            if !simplified_return_types.is_empty() {
                return simplified_return_types
                    .into_iter()
                    .map(|ret| Type::Function(arg_types.clone(), Box::new(ret)))
                    .collect();
            }

            // now check the arguments, and see if we can simplify them in a
            // better way.
            for idx in 0..arg_types.len() {
                let simplified_arguments = simplify_type(&arg_types[idx]);

                if simplified_arguments.is_empty() {
                    continue;
                }

                let mut new_function_types = vec![];

                for simplified_arg in simplified_arguments.into_iter() {
                    let mut new_args = vec![];

                    for item in &arg_types[0..idx] {
                        new_args.push(item.clone());
                    }
                    new_args.push(simplified_arg);
                    for item in &arg_types[idx + 1..arg_types.len()] {
                        new_args.push(item.clone());
                    }

                    new_function_types.push(Type::Function(new_args, ret_type.clone()));
                }

                if !new_function_types.is_empty() {
                    return new_function_types;
                }
            }

            // ok, all of the arguments and the return type are already as
            // simple as they can be, so let's see if we can reduce the number
            // of arguments.
            let mut new_types = vec![];
            for args in arg_types.iter().powerset() {
                if args.len() != arg_types.len() {
                    new_types.push(Type::Function(
                        args.into_iter().cloned().collect(),
                        ret_type.clone(),
                    ));
                }
            }

            if new_types.is_empty() {
                vec![ret_type.as_ref().clone()]
            } else {
                new_types
            }
        }

        Type::Application(constructor_type, arg_types) => {
            // much like functions, we're going to try to simplify the constructor,
            // then we'll try to simplify the arguments, then we'll try to remove
            // arguments.
            let simplified_constructor = simplify_type(constructor_type.as_ref());

            if !simplified_constructor.is_empty() {
                return simplified_constructor
                    .into_iter()
                    .map(|c| Type::Application(Box::new(c), arg_types.clone()))
                    .collect();
            }

            // now check the arguments, and see if we can simplify them in a
            // better way.
            for idx in 0..arg_types.len() {
                let simplified_arguments = simplify_type(&arg_types[idx]);

                if simplified_arguments.is_empty() {
                    continue;
                }

                let mut new_appl_types = vec![];

                for simplified_arg in simplified_arguments.into_iter() {
                    let mut new_args = vec![];

                    for item in &arg_types[0..idx] {
                        new_args.push(item.clone());
                    }
                    new_args.push(simplified_arg);
                    for item in &arg_types[idx + 1..arg_types.len()] {
                        new_args.push(item.clone());
                    }

                    new_appl_types.push(Type::Application(constructor_type.clone(), new_args));
                }

                if !new_appl_types.is_empty() {
                    return new_appl_types;
                }
            }

            // and now we'll try to reduce types.
            let mut new_types = vec![];
            for args in arg_types.iter().powerset() {
                if args.len() != arg_types.len() {
                    new_types.push(Type::Application(
                        constructor_type.clone(),
                        args.into_iter().cloned().collect(),
                    ));
                }
            }

            if new_types.is_empty() {
                vec![constructor_type.as_ref().clone()]
            } else {
                new_types
            }
        }
    }
}

impl ValueTree for TypeGenerationTree {
    type Value = Type;

    fn current(&self) -> Self::Value {
        self.current_value.clone()
    }

    fn simplify(&mut self) -> bool {
        match self.untried_simplified_items.as_mut() {
            None => {
                let mut simplified = simplify_type(&self.current_value)
                    .into_iter()
                    .map(|current_value| TypeGenerationTree {
                        current_value,
                        parent: Some(Box::new(self.clone())),

                        untried_simplified_items: None,
                    })
                    .collect::<Vec<_>>();

                match simplified.pop() {
                    None => {
                        self.untried_simplified_items = Some(simplified);
                        false
                    }

                    Some(next_tree) => {
                        self.untried_simplified_items = Some(simplified);
                        *self = next_tree;
                        true
                    }
                }
            }

            Some(untried_simplifieds) => match untried_simplifieds.pop() {
                None => false,
                Some(x) => {
                    *self = x;
                    true
                }
            },
        }
    }

    fn complicate(&mut self) -> bool {
        match self.parent.take() {
            None => false,
            Some(x) => {
                *self = *x;
                true
            }
        }
    }
}

impl Strategy for TypeGenerationContext {
    type Tree = TypeGenerationTree;
    type Value = Type;

    fn new_tree(&self, _runner: &mut TestRunner) -> NewTree<Self> {
        unimplemented!()
    }
}

impl Arbitrary for Type {
    type Parameters = TypeGenerationContext;
    type Strategy = TypeGenerationContext;

    fn arbitrary_with(_context: Self::Parameters) -> Self::Strategy {
        unimplemented!()
    }
}

#[derive(Default)]
pub enum LegalConstantType {
    #[default]
    Any,
    String,
    Char,
    Number,
}

impl Arbitrary for ConstantValue {
    type Parameters = LegalConstantType;
    type Strategy = BoxedStrategy<ConstantValue>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        match args {
            LegalConstantType::Char => char::arbitrary()
                .prop_map(|x| ConstantValue::Character(Location::manufactured(), x))
                .boxed(),

            LegalConstantType::String => {
                proptest::collection::vec(proptest::char::any(), MAXIMUM_STRING_SIZE)
                    .prop_map(|x| {
                        ConstantValue::String(Location::manufactured(), String::from_iter(x))
                    })
                    .boxed()
            }

            LegalConstantType::Number => {
                let value_strat = u64::arbitrary();
                let base_strat = proptest::prop_oneof![
                    proptest::strategy::Just(None),
                    proptest::strategy::Just(Some(2)),
                    proptest::strategy::Just(Some(8)),
                    proptest::strategy::Just(Some(10)),
                    proptest::strategy::Just(Some(16)),
                ];

                (value_strat, base_strat)
                    .prop_map(|(value, base)| {
                        ConstantValue::Integer(
                            Location::manufactured(),
                            IntegerWithBase { base, value },
                        )
                    })
                    .boxed()
            }

            LegalConstantType::Any => proptest::prop_oneof![
                Self::arbitrary_with(LegalConstantType::Char),
                Self::arbitrary_with(LegalConstantType::String),
                Self::arbitrary_with(LegalConstantType::Number),
            ]
            .boxed(),
        }
    }
}

#[cfg(test)]
mod simplifiers {
    use super::*;

    #[test]
    fn types() {
        let loc = Location::manufactured();
        let foo = Name::new(loc.clone(), "Foo");
        let primint = Type::Primitive(loc.clone(), Name::new(loc.clone(), "Int"));
        let primchar = Type::Primitive(loc.clone(), Name::new(loc.clone(), "Char"));
        let primstr = Type::Primitive(loc.clone(), Name::new(loc.clone(), "String"));

        assert_eq!(
            simplify_type(&Type::Constructor(loc.clone(), foo.clone())),
            vec![]
        );
        assert_eq!(
            simplify_type(&Type::Variable(loc.clone(), foo.clone())),
            vec![]
        );
        assert_eq!(
            simplify_type(&Type::Primitive(loc.clone(), foo.clone())),
            vec![]
        );

        assert_eq!(
            simplify_type(&Type::Function(vec![], Box::new(primint.clone()))),
            vec![primint.clone()]
        );
        assert_eq!(
            simplify_type(&Type::Function(
                vec![primint.clone(), primchar.clone()],
                Box::new(primint.clone())
            )),
            vec![
                Type::Function(vec![], Box::new(primint.clone())),
                Type::Function(vec![primint.clone()], Box::new(primint.clone())),
                Type::Function(vec![primchar.clone()], Box::new(primint.clone())),
            ]
        );
        assert_eq!(
            simplify_type(&Type::Function(
                vec![primint.clone(), primchar.clone(), primstr.clone()],
                Box::new(primint.clone())
            )),
            vec![
                Type::Function(vec![], Box::new(primint.clone())),
                Type::Function(vec![primint.clone()], Box::new(primint.clone())),
                Type::Function(vec![primchar.clone()], Box::new(primint.clone())),
                Type::Function(vec![primstr.clone()], Box::new(primint.clone())),
                Type::Function(
                    vec![primint.clone(), primchar.clone()],
                    Box::new(primint.clone())
                ),
                Type::Function(
                    vec![primint.clone(), primstr.clone()],
                    Box::new(primint.clone())
                ),
                Type::Function(
                    vec![primchar.clone(), primstr.clone()],
                    Box::new(primint.clone())
                ),
            ]
        );

        assert_eq!(
            simplify_type(&Type::Function(
                vec![primint.clone(), primchar.clone(), primstr.clone()],
                Box::new(Type::Function(vec![], Box::new(primint.clone()))),
            )),
            vec![Type::Function(
                vec![primint.clone(), primchar.clone(), primstr.clone()],
                Box::new(primint.clone())
            ),]
        );
        assert_eq!(
            simplify_type(&Type::Function(
                vec![primint.clone(), primchar.clone(), primstr.clone()],
                Box::new(Type::Function(
                    vec![primint.clone(), primchar.clone()],
                    Box::new(primint.clone())
                )),
            )),
            vec![
                Type::Function(
                    vec![primint.clone(), primchar.clone(), primstr.clone()],
                    Box::new(Type::Function(vec![], Box::new(primint.clone())))
                ),
                Type::Function(
                    vec![primint.clone(), primchar.clone(), primstr.clone()],
                    Box::new(Type::Function(
                        vec![primint.clone()],
                        Box::new(primint.clone())
                    ))
                ),
                Type::Function(
                    vec![primint.clone(), primchar.clone(), primstr.clone()],
                    Box::new(Type::Function(
                        vec![primchar.clone()],
                        Box::new(primint.clone())
                    ))
                ),
            ]
        );
        assert_eq!(
            simplify_type(&Type::Function(
                vec![
                    Type::Function(vec![], Box::new(primint.clone())),
                    primstr.clone()
                ],
                Box::new(primint.clone())
            )),
            vec![Type::Function(
                vec![primint.clone(), primstr.clone()],
                Box::new(primint.clone())
            )]
        );
        assert_eq!(
            simplify_type(&Type::Function(
                vec![
                    primint.clone(),
                    Type::Function(vec![], Box::new(primint.clone()))
                ],
                Box::new(primint.clone())
            )),
            vec![Type::Function(
                vec![primint.clone(), primint.clone()],
                Box::new(primint.clone())
            )]
        );

        let applied = Type::Application(Box::new(primint.clone()), vec![]);
        assert_eq!(
            simplify_type(&Type::Application(Box::new(primint.clone()), vec![])),
            vec![primint.clone()]
        );
        assert_eq!(simplify_type(&applied), vec![primint.clone()]);
        assert_eq!(
            simplify_type(&Type::Application(
                Box::new(applied.clone()),
                vec![primint.clone()]
            )),
            vec![Type::Application(
                Box::new(primint.clone()),
                vec![primint.clone()]
            )]
        );
        assert_eq!(
            simplify_type(&Type::Application(
                Box::new(primint.clone()),
                vec![applied.clone()]
            )),
            vec![Type::Application(
                Box::new(primint.clone()),
                vec![primint.clone()]
            )]
        );
        assert_eq!(
            simplify_type(&Type::Application(
                Box::new(primint.clone()),
                vec![primchar.clone(), applied.clone(), primstr.clone()]
            )),
            vec![Type::Application(
                Box::new(primint.clone()),
                vec![primchar.clone(), primint.clone(), primstr.clone()]
            )]
        );
        assert_eq!(
            simplify_type(&Type::Application(
                Box::new(primint.clone()),
                vec![primchar.clone(), primint.clone(), primstr.clone()]
            )),
            vec![
                Type::Application(Box::new(primint.clone()), vec![]),
                Type::Application(Box::new(primint.clone()), vec![primchar.clone()]),
                Type::Application(Box::new(primint.clone()), vec![primint.clone()]),
                Type::Application(Box::new(primint.clone()), vec![primstr.clone()]),
                Type::Application(
                    Box::new(primint.clone()),
                    vec![primchar.clone(), primint.clone()]
                ),
                Type::Application(
                    Box::new(primint.clone()),
                    vec![primchar.clone(), primstr.clone()]
                ),
                Type::Application(
                    Box::new(primint.clone()),
                    vec![primint.clone(), primstr.clone()]
                )
            ]
        );
    }
}
