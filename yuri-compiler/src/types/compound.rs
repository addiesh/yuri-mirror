use std::borrow::Cow;

use crate::types::{TypeValue, Typeable};
use crate::{Attribute, CompileError, split_results};

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundType<'a> {
    pub fields: Vec<CompoundTypeField<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundTypeField<'a> {
    // pub parent: Weak<CompoundType<'a>>,
    pub attributes: Vec<Attribute<'a>>,
    pub name: Cow<'a, str>,
    pub field_type: TypeValue<'a>,
}

impl<'a> Typeable<'a> for CompoundType<'a> {
    fn intersect_with(&self, other: &Self) -> Result<Self, CompileError<'a>> {
        if self.fields.len() != other.fields.len() {
            let l1 = self.fields.len() as u32;
            let l2 = other.fields.len() as u32;
            return Err(CompileError::IncompatibleFieldCount(l1, l2));
        }

        // should we quack based on field order, field name, or both?
        let results: Vec<Result<_, CompileError>> = self
            .fields
            .iter()
            .zip(&other.fields)
            .map(|(a, b)| {
                // bias towards target
                Ok(CompoundTypeField {
                    field_type: a.field_type.intersect_with(&b.field_type)?,
                    // parent: b.parent.clone(),
                    attributes: b.attributes.clone(),
                    name: b.name.clone(),
                })
            })
            .collect();

        let (fields, errors) = split_results(results.into_iter());
        if !errors.is_empty() {
            return Err(CompileError::Multiple(errors));
        }

        Ok(CompoundType { fields })
    }

    fn union(this: &Self, other: &Self) -> Result<Self, CompileError<'a>> {
        todo!()
    }

    fn is_resolved(&self) -> bool {
        self.fields
            .iter()
            .all(|field| field.field_type.is_resolved())
    }

    fn try_resolve(&self) -> Result<Self, CompileError<'a>> {
        let (fields, errors) = split_results(self.fields.iter().map(|field| {
            Ok(CompoundTypeField {
                attributes: field.attributes.clone(),
                name: field.name.clone(),
                field_type: field.field_type.try_resolve()?,
            })
        }));
        if !errors.is_empty() {
            Err(CompileError::Multiple(errors))
        } else {
            Ok(Self { fields })
        }
    }
}
