use crate::variable::{RcObj, Env};
use crate::error::Result;

pub mod function;
pub mod class;

pub trait Interpretable {
    fn interpret(&self, env: Env) -> Result<RcObj>;
}