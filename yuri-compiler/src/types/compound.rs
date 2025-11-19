use yuri_parser::Ident;

use crate::attribute::Attribute;
use crate::error::{CompileError, TypeError};
use crate::split_results;
use crate::types::{TypeValue, Typeable};

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundType<'a> {
    pub fields: Vec<CompoundTypeField<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundTypeField<'a> {
    // pub parent: Ywk<CompoundType<'a>>,
    pub attributes: Vec<Attribute<'a>>,
    pub name: Ident,
    pub field_type: TypeValue<'a>,
}

impl<'a> Typeable<'a> for CompoundType<'a> {
    fn intersect_with(&self, other: &Self) -> Result<Self, TypeError<'a>> {
        if self.fields.len() != other.fields.len() {
            let l1 = self.fields.len() as u32;
            let l2 = other.fields.len() as u32;
            return Err(TypeError::IncompatibleFieldCount(l1, l2));
        }

        // should we quack based on field order, field name, or both?
        let results: Vec<Result<_, TypeError>> = self
            .fields
            .iter()
            .zip(&other.fields)
            .map(|(a, b)| {
                // bias towards target
                Ok(CompoundTypeField {
                    field_type: a.field_type.intersect_with(&b.field_type)?,
                    // parent: b.parent.clone(),
                    attributes: b.attributes.clone(),
                    name: b.name,
                })
            })
            .collect();

        let (fields, errors) = split_results(results.into_iter());
        if !errors.is_empty() {
            return Err(TypeError::Multiple(errors));
        }

        Ok(CompoundType { fields })
    }

    fn union(this: &Self, other: &Self) -> Result<Self, TypeError<'a>> {
        todo!()
    }

    fn is_resolved(&self) -> bool {
        self.fields
            .iter()
            .all(|field| field.field_type.is_resolved())
    }
}
