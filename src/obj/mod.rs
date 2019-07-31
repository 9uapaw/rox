use crate::variable::{RcObj, Env};
use crate::error::Result;

pub mod function;

pub trait Object {
    fn interpret(&self, env: Env) -> Result<RcObj>;
}