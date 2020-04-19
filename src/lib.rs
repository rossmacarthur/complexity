//! Calculate cognitive complexity of Rust code.
//!
//! # Examples
//!
//! ```
//! # fn main() -> syn::Result<()> {
//! use complexity::Complexity;
//! use syn::{ItemFn, parse_str};
//!
//! let code = r#"
//! fn sum_of_primes(max: u64) -> u64 {
//!     let mut total = 0;
//!     'outer: for i in 1..=max {
//!         for j in 2..i {
//!             if i % j == 0 {
//!                 continue 'outer;
//!             }
//!         }
//!         total += i;
//!     }
//! }"#;
//!
//! let func: ItemFn = parse_str(code)?;
//! assert_eq!(func.complexity(), 7);
//! #
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! # fn main() -> syn::Result<()> {
//! use complexity::Complexity;
//! use syn::{ItemFn, parse_str};
//!
//! let code = r#"
//! fn get_words(number: u64) -> &'static str {
//!     match number {
//!         1 => "one",
//!         2 => "a couple",
//!         3 => "a few",
//!         _ => "lots",
//!     }
//! }"#;
//!
//! let func: ItemFn = parse_str(code)?;
//! assert_eq!(func.complexity(), 1);
//! #
//! # Ok(())
//! # }

use syn::*;

mod private {
    pub trait Sealed {}
    impl Sealed for super::ItemFn {}
    impl Sealed for super::ImplItemMethod {}
}

/// A trait for calculating the cognitive complexity of a Rust type.
///
/// This is a *sealed* trait so only this crate can implement it.
pub trait Complexity: private::Sealed {
    /// Returns the cognitive complexity index for the implementor.
    fn complexity(&self) -> u64;
}

impl Complexity for ItemFn {
    fn complexity(&self) -> u64 {
        unimplemented!()
    }
}

impl Complexity for ImplItemMethod {
    fn complexity(&self) -> u64 {
        unimplemented!()
    }
}
