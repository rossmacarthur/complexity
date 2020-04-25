//! Calculate cognitive complexity of Rust code.
//!
//! Based on [Cognitive Complexity][pdf] by G. Ann Campbell.
//!
//! [pdf]: https://www.sonarsource.com/docs/CognitiveComplexity.pdf
//!
//! # Examples
//!
//! Loops and branching increment the complexity by one each. Additionally,
//! `continue` and `break` statements increment the complexity by one.
//!
//! ```
//! use complexity::Complexity;
//! use syn::{ItemFn, parse_quote};
//!
//! let func: ItemFn = parse_quote! {
//!     fn sum_of_primes(max: u64) -> u64 {
//!         let mut total = 0;
//!         'outer: for i in 1..=max {
//!             for j in 2..i {
//!                 if i % j == 0 {
//!                     continue 'outer;
//!                 }
//!             }
//!             total += i;
//!         }
//!     }
//! };
//! assert_eq!(func.complexity(), 7);
//! ```
//!
//! However, a `match` only increases the complexity by one no matter how many
//! branches there are.
//!
//! ```
//! use complexity::Complexity;
//! use syn::{ItemFn, parse_quote};
//!
//! let func: ItemFn = parse_quote! {
//!     fn get_words(number: u64) -> &str {
//!         match number {
//!             1 => "one",
//!             2 => "a couple",
//!             3 => "a few",
//!             _ => "lots",
//!         }
//!     }
//! };
//! assert_eq!(func.complexity(), 1);
//! ```

use std::{iter, ops};

use syn::*;

/////////////////////////////////////////////////////////////////////////
// Complexity trait
/////////////////////////////////////////////////////////////////////////

mod private {
    pub trait Sealed {}
    impl Sealed for super::Expr {}
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

impl Complexity for Expr {
    fn complexity(&self) -> u64 {
        eval_expr(self, Nesting::zero()).0.into()
    }
}

impl Complexity for ItemFn {
    fn complexity(&self) -> u64 {
        eval_block(&self.block, Nesting::zero()).0.into()
    }
}

impl Complexity for ImplItemMethod {
    fn complexity(&self) -> u64 {
        eval_block(&self.block, Nesting::zero()).0.into()
    }
}

/////////////////////////////////////////////////////////////////////////
// Index type
/////////////////////////////////////////////////////////////////////////

#[derive(Debug, Copy, Clone)]
struct Index(u32);

impl Index {
    /// Construct a new zero `Index`.
    #[inline]
    fn zero() -> Self {
        Self(0)
    }

    /// Construct a new `Index` of one.
    #[inline]
    fn one() -> Self {
        Self(1)
    }

    /// Construct a new `Index` based on the Nesting.
    #[inline]
    fn with_nesting(n: Nesting) -> Self {
        Self(1 + n.0)
    }
}

impl ops::Add for Index {
    type Output = Self;

    /// Add one `Index` to another.
    #[inline]
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

impl iter::Sum<Index> for Index {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        iter.fold(Self::zero(), ops::Add::add)
    }
}

/////////////////////////////////////////////////////////////////////////
// Nesting type
/////////////////////////////////////////////////////////////////////////

#[derive(Debug, Copy, Clone)]
struct Nesting(u32);

impl Nesting {
    #[inline]
    fn zero() -> Self {
        Self(0)
    }

    #[inline]
    fn increase(self) -> Self {
        Self(self.0 + 1)
    }
}

/////////////////////////////////////////////////////////////////////////
// Evaluation functions
/////////////////////////////////////////////////////////////////////////

/// Returns the complexity of a `syn::Block`.
fn eval_block(block: &Block, nesting: Nesting) -> Index {
    block
        .stmts
        .iter()
        .map(|e| eval_stmt(e, nesting))
        .sum::<Index>()
}

/// Returns the complexity of a `syn::Stmt`.
fn eval_stmt(stmt: &Stmt, nesting: Nesting) -> Index {
    match stmt {
        Stmt::Local(Local {
            init: Some((_, expr)),
            ..
        }) => eval_expr(expr, nesting),
        Stmt::Local(Local { init: None, .. }) => Index::zero(),
        Stmt::Item(item) => eval_item(item, nesting),
        Stmt::Expr(expr) | Stmt::Semi(expr, _) => eval_expr(expr, nesting),
    }
}

/// Returns the complexity of a `syn::Item`.
fn eval_item(item: &Item, n: Nesting) -> Index {
    match item {
        Item::Const(ItemConst { expr, .. }) => eval_expr(expr, n),
        Item::Static(ItemStatic { expr, .. }) => eval_expr(expr, n),
        _ => Index::zero(),
    }
}

/// Returns the complexity of a `syn::Expr`.
///
/// This function contains most of the logic for calculating cognitive
/// complexity. Expressions that create nesting increase the complexity and
/// expressions that increase the branching increasing the complexity.
fn eval_expr(expr: &Expr, nesting: Nesting) -> Index {
    match expr {
        // Expressions that map to multiple expressions.
        // --------------------------------------------
        Expr::Array(ExprArray { elems, .. }) | Expr::Tuple(ExprTuple { elems, .. }) => {
            elems.iter().map(|e| eval_expr(e, nesting)).sum()
        }

        // Expressions that have a left and right part.
        // --------------------------------------------
        Expr::Assign(ExprAssign { left, right, .. })
        | Expr::AssignOp(ExprAssignOp { left, right, .. })
        | Expr::Binary(ExprBinary { left, right, .. })
        | Expr::Index(ExprIndex {
            expr: left,
            index: right,
            ..
        })
        | Expr::Repeat(ExprRepeat {
            expr: left,
            len: right,
            ..
        }) => eval_expr(left, nesting) + eval_expr(right, nesting),

        Expr::Range(ExprRange { from, to, .. }) => {
            from.as_ref()
                .map(|e| eval_expr(e, nesting))
                .unwrap_or_else(Index::zero)
                + to.as_ref()
                    .map(|e| eval_expr(e, nesting))
                    .unwrap_or_else(Index::zero)
        }

        // Expressions that create a nested block like `async { .. }`.
        // ---------------------------------------------------
        Expr::Async(ExprAsync { block, .. })
        | Expr::Block(ExprBlock { block, .. })
        | Expr::Loop(ExprLoop { body: block, .. })
        | Expr::TryBlock(ExprTryBlock { block, .. })
        | Expr::Unsafe(ExprUnsafe { block, .. }) => eval_block(block, nesting.increase()),

        // Expressions that wrap a single expression.
        // ------------------------------------------
        Expr::Await(ExprAwait { base: expr, .. })
        | Expr::Box(ExprBox { expr, .. })
        | Expr::Break(ExprBreak {
            expr: Some(expr), ..
        })
        | Expr::Cast(ExprCast { expr, .. })
        | Expr::Closure(ExprClosure { body: expr, .. })
        | Expr::Field(ExprField { base: expr, .. })
        | Expr::Group(ExprGroup { expr, .. })
        | Expr::Let(ExprLet { expr, .. })
        | Expr::Paren(ExprParen { expr, .. })
        | Expr::Reference(ExprReference { expr, .. })
        | Expr::Return(ExprReturn {
            expr: Some(expr), ..
        })
        | Expr::Try(ExprTry { expr, .. })
        | Expr::Type(ExprType { expr, .. })
        | Expr::Unary(ExprUnary { expr, .. })
        | Expr::Yield(ExprYield {
            expr: Some(expr), ..
        }) => eval_expr(expr, nesting),

        // Expressions that introduce branching.
        // ------------------------------------
        Expr::If(ExprIf {
            cond,
            then_branch,
            else_branch,
            ..
        }) => {
            Index::with_nesting(nesting)
                + eval_expr(cond, nesting)
                + eval_block(then_branch, nesting.increase())
                + else_branch
                    .as_ref()
                    .map(|(_, expr)| Index::one() + eval_expr(&expr, nesting.increase()))
                    .unwrap_or_else(Index::zero)
        }
        Expr::Match(ExprMatch { expr, arms, .. }) => {
            Index::with_nesting(nesting)
                + eval_expr(expr, nesting)
                + arms
                    .iter()
                    .map(|arm| {
                        eval_opt_t_expr(&arm.guard, nesting)
                            + eval_expr(&arm.body, nesting.increase())
                    })
                    .sum::<Index>()
        }
        Expr::ForLoop(ExprForLoop { expr, body, .. }) => {
            Index::with_nesting(nesting)
                + eval_expr(expr, nesting)
                + eval_block(body, nesting.increase())
        }
        Expr::While(ExprWhile { cond, body, .. }) => {
            Index::with_nesting(nesting)
                + eval_expr(cond, nesting)
                + eval_block(body, nesting.increase())
        }
        Expr::Continue(_) | Expr::Break(_) => Index::one(),

        // Expressions that call functions / construct types.
        // -------------------------------------------------
        Expr::Struct(ExprStruct { fields, rest, .. }) => {
            fields
                .iter()
                .map(|v| eval_expr(&v.expr, nesting))
                .sum::<Index>()
                + rest
                    .as_ref()
                    .map(|e| eval_expr(e, nesting))
                    .unwrap_or_else(Index::zero)
        }
        Expr::Call(ExprCall { func, args, .. }) => {
            eval_expr(func, nesting) + args.iter().map(|a| eval_expr(a, nesting)).sum::<Index>()
        }
        Expr::MethodCall(ExprMethodCall { receiver, args, .. }) => {
            eval_expr(receiver, nesting) + args.iter().map(|a| eval_expr(a, nesting)).sum::<Index>()
        }

        _ => Index::zero(),
    }
}

/// Returns the complexity of a optional `syn::Expr`.
fn eval_opt_t_expr<T>(opt_expr: &Option<(T, Box<Expr>)>, nesting: Nesting) -> Index {
    opt_expr
        .as_ref()
        .map(|(_, expr)| eval_expr(expr, nesting))
        .unwrap_or_else(Index::zero)
}

/////////////////////////////////////////////////////////////////////////
// Unit tests
/////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn if_statement() {
        let expr: Expr = parse_quote! {
            if true {             // +1
                println!("test");
            }
        };
        assert_eq!(expr.complexity(), 1);
    }

    #[test]
    fn if_statement_nesting_increment() {
        let expr: Expr = parse_quote! {
            if true {                 // +1
                if true {             // +2 (nesting = 1)
                    println!("test");
                }
            }
        };
        assert_eq!(expr.complexity(), 3);
    }

    #[test]
    fn if_else_statement_no_nesting_increment() {
        let expr: Expr = parse_quote! {
            if true {                 // +1
                if true {             // +2 (nesting = 1)
                    println!("test");
                } else {              // +1
                    println!("test");
                }
            }
        };
        assert_eq!(expr.complexity(), 4);
    }

    #[test]
    fn for_loop() {
        let expr: Expr = parse_quote! {
            for element in iterable { // +1
                if true {             // +2 (nesting = 1)
                    println!("test");
                }
            }
        };
        assert_eq!(expr.complexity(), 3);
    }

    #[test]
    fn for_loop_nesting_increment() {
        let expr: Expr = parse_quote! {
            if true {                     // +1
                for element in iterable { // +2 (nesting = 1)
                    println!("test");
                }
            }
        };
        assert_eq!(expr.complexity(), 3);
    }

    #[test]
    fn while_loop() {
        let expr: Expr = parse_quote! {
            while true {              // +1
                if true {             // +2 (nesting = 1)
                    println!("test");
                }
            }
        };
        assert_eq!(expr.complexity(), 3);
    }

    #[test]
    fn while_loop_nesting_increment() {
        let expr: Expr = parse_quote! {
            if true {                 // +1
                while true {          // +2 (nesting = 1)
                    println!("test");
                }
            }
        };
        assert_eq!(expr.complexity(), 3);
    }

    #[test]
    fn match_statement_nesting_increment() {
        let expr: Expr = parse_quote! {
            if true {                          // +1
                match true {                   // +2 (nesting = 1)
                    true => println!("test"),
                    false => println!("test"),
                }
            }
        };
        assert_eq!(expr.complexity(), 3);
    }
}
