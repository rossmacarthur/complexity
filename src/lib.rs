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
//! However, a `match` only increases the complexity by one no matter how many
//! branches there are.
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
        eval_block(&self.block)
    }
}

impl Complexity for ImplItemMethod {
    fn complexity(&self) -> u64 {
        eval_block(&self.block)
    }
}

/// Returns the complexity of a `syn::Block`.
fn eval_block(block: &Block) -> u64 {
    block.stmts.iter().map(eval_stmt).sum::<u64>()
}

/// Returns the complexity of a `syn::Stmt`.
fn eval_stmt(stmt: &Stmt) -> u64 {
    match stmt {
        Stmt::Local(Local {
            init: Some((_, expr)),
            ..
        }) => eval_expr(expr),
        Stmt::Local(Local { init: None, .. }) => 0,
        Stmt::Item(item) => eval_item(item),
        Stmt::Expr(expr) | Stmt::Semi(expr, _) => eval_expr(expr),
    }
}

/// Returns the complexity of a `syn::Item`.
fn eval_item(item: &Item) -> u64 {
    match item {
        Item::Const(ItemConst { expr, .. }) => eval_expr(expr),
        Item::Static(ItemStatic { expr, .. }) => eval_expr(expr),
        _ => 0,
    }
}

/// Returns the complexity of a `syn::Expr`.
///
/// This function contains most of the logic for calculating cognitive
/// complexity. Expressions that create nesting increase the complexity and
/// expressions that increase the branching increasing the complexity.
fn eval_expr(expr: &Expr) -> u64 {
    match expr {
        // Expressions that map to multiple expressions.
        // --------------------------------------------
        Expr::Array(ExprArray { elems, .. }) | Expr::Tuple(ExprTuple { elems, .. }) => {
            elems.iter().map(eval_expr).sum()
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
        }) => eval_expr(left) + eval_expr(right),

        Expr::Range(ExprRange { from, to, .. }) => {
            from.as_ref().map(|e| eval_expr(e)).unwrap_or(0)
                + to.as_ref().map(|e| eval_expr(e)).unwrap_or(0)
        }

        // Expressions that create a nested block like `async { .. }`.
        // ---------------------------------------------------
        Expr::Async(ExprAsync { block, .. })
        | Expr::Block(ExprBlock { block, .. })
        | Expr::Loop(ExprLoop { body: block, .. })
        | Expr::TryBlock(ExprTryBlock { block, .. })
        | Expr::Unsafe(ExprUnsafe { block, .. }) => 1 + eval_block(block),

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
        }) => eval_expr(expr),

        // Expressions that introduce branching.
        // ------------------------------------
        Expr::ForLoop(ExprForLoop { expr, body, .. }) => 2 + eval_expr(expr) + eval_block(body),
        Expr::While(ExprWhile { cond, body, .. }) => 2 + eval_expr(cond) + eval_block(body),
        Expr::If(ExprIf {
            cond,
            then_branch,
            else_branch,
            ..
        }) => 2 + eval_expr(cond) + eval_block(then_branch) + eval_opt_t_expr(else_branch),
        Expr::Match(ExprMatch { expr, arms, .. }) => {
            1 + eval_expr(expr)
                + arms
                    .iter()
                    .map(|arm| eval_opt_t_expr(&arm.guard) + eval_expr(&arm.body))
                    .sum::<u64>()
        }
        Expr::Continue(_) | Expr::Break(_) => 1,

        // Expressions that call functions / construct types.
        // -------------------------------------------------
        Expr::Struct(ExprStruct { fields, rest, .. }) => {
            fields.iter().map(|v| eval_expr(&v.expr)).sum::<u64>()
                + rest.as_ref().map(|e| eval_expr(e)).unwrap_or(0)
        }
        Expr::Call(ExprCall { func, args, .. }) => {
            eval_expr(func) + args.iter().map(eval_expr).sum::<u64>()
        }
        Expr::MethodCall(ExprMethodCall { receiver, args, .. }) => {
            eval_expr(receiver) + args.iter().map(eval_expr).sum::<u64>()
        }

        _ => 0,
    }
}

/// Returns the complexity of a optional `syn::Expr`.
fn eval_opt_t_expr<T>(opt_expr: &Option<(T, Box<Expr>)>) -> u64 {
    opt_expr
        .as_ref()
        .map(|(_, expr)| eval_expr(expr))
        .unwrap_or(0)
}
