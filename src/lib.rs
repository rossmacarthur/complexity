//! Calculate cognitive complexity of Rust code.
//!
//! Based on [Cognitive Complexity][pdf] by G. Ann Campbell.
//!
//! ## Getting started
//!
//! You'll need to bring the [`Complexity`] trait into scope, and probably some
//! things from [`syn`].
//!
//! ```rust
//! use complexity::Complexity;
//! use syn::{Expr, parse_quote};
//! ```
//!
//! Complexity of expressions and other [`syn`] types is as simple as calling
//! [`.complexity()`] on an instance of that type.
//!
//! ```rust
//! # use complexity::Complexity;
//! # use syn::{Expr, parse_quote};
//! let expr: Expr = parse_quote! {
//!     for element in iterable { // +1
//!         if something {        // +2 (nesting = 1)
//!             do_something();
//!         }
//!     }
//! };
//! assert_eq!(expr.complexity(), 3);
//! ```
//!
//! ## Examples
//!
//! The implementation of cognitive complexity in this crate is heavily based on
//! [Cognitive Complexity][pdf] by G. Ann Campbell. And reading it would be
//! beneficial to understanding how the complexity index is calculated.
//!
//! Loops and structures that introduce branching increment the complexity by
//! one each. Some syntax structures introduce a "nesting" level which increases
//! some expressions complexity by that nesting level in addition to their
//! regular increment. In the example below we see how two nested loops and an
//! if statement can produce quite a high complexity of **7**.
//!
//! ```rust
//! use complexity::Complexity;
//! use syn::{ItemFn, parse_quote};
//!
//! let func: ItemFn = parse_quote! {
//!     fn sum_of_primes(max: u64) -> u64 {
//!         let mut total = 0;
//!         'outer: for i in 1..=max {   // +1
//!             for j in 2..i {          // +2 (nesting = 1)
//!                 if i % j == 0 {      // +3 (nesting = 2)
//!                     continue 'outer; // +1
//!                 }
//!             }
//!             total += i;
//!         }
//!     }
//! };
//! assert_eq!(func.complexity(), 7);
//! ```
//!
//! But some structures are rewarded. Particularly a `match` statement, which
//! only increases the complexity by one no matter how many branches there are.
//! (It does increase the nesting level though.) In the example below we see how
//! even though there are a lot of branches in the code (which would contribute
//! a lot to a more traditional *cyclomatic complexity* measurement), the
//! complexity is quite low at **1**.
//!
//! ```rust
//! use complexity::Complexity;
//! use syn::{ItemFn, parse_quote};
//!
//! let func: ItemFn = parse_quote! {
//!     fn get_words(number: u64) -> &str {
//!         match number {       // +1
//!             1 => "one",
//!             2 => "a couple",
//!             3 => "a few",
//!             _ => "lots",
//!         }
//!     }
//! };
//! assert_eq!(func.complexity(), 1);
//! ```
//!
//! An example is provided to calculate and nicely print out the cognitive
//! complexity of each function and method in an entire Rust file. See
//! [examples/lint-files.rs](examples/lint-files.rs). You can run it on Rust
//! files like this:
//!
//! ```sh
//! cargo run --example lint-files -- src/
//! ```
//!
//! [pdf]: https://www.sonarsource.com/docs/CognitiveComplexity.pdf
//! [`Complexity`]: trait.Complexity.html
//! [`.complexity()`]: trait.Complexity.html#tymethod.complexity
//! [`syn`]: https://docs.rs/syn/1

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
    fn complexity(&self) -> u32;
}

impl Complexity for Expr {
    fn complexity(&self) -> u32 {
        eval_expr(self, State::default()).0
    }
}

impl Complexity for ItemFn {
    fn complexity(&self) -> u32 {
        eval_block(&self.block, State::default()).0
    }
}

impl Complexity for ImplItemMethod {
    fn complexity(&self) -> u32 {
        eval_block(&self.block, State::default()).0
    }
}

/////////////////////////////////////////////////////////////////////////
// Index type
/////////////////////////////////////////////////////////////////////////

/// Represents a complexity index.
#[derive(Debug, Clone, Copy)]
struct Index(u32);

impl Index {
    /// Construct a new zero `Index` that does not contribute to complexity.
    fn zero() -> Self {
        Self(0)
    }

    /// Construct a new `Index` that adds one to the complexity.
    fn one() -> Self {
        Self(1)
    }

    /// Construct a new `Index` that adds one to the complexity and one for each
    /// level of nesting.
    fn with_context(state: State) -> Self {
        Self(state.nesting + 1)
    }
}

impl ops::Add for Index {
    type Output = Self;

    /// Add one `Index` to another.
    ///
    /// This simply is the addition of both complexities.
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

impl iter::Sum<Index> for Index {
    /// Sum an iterable of `Index` by simply accumulating all the complexities
    /// into one.
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        iter.fold(Self::zero(), ops::Add::add)
    }
}

/////////////////////////////////////////////////////////////////////////
// Logical boolean operator type
/////////////////////////////////////////////////////////////////////////

/// Represents a logical boolean operator.
#[derive(Debug, Clone, Copy, PartialEq)]
enum LogBoolOp {
    /// The `!` operator (logical not).
    Not,
    /// The `&&` operator (logical and).
    And,
    /// The `||` operator (logical or).
    Or,
}

impl LogBoolOp {
    /// Create a new `LogBoolOp` from a `syn::UnOp`.
    fn from_un_op(un_op: UnOp) -> Option<Self> {
        match un_op {
            UnOp::Not(_) => Some(Self::Not),
            _ => None,
        }
    }

    /// Create a new `LogBoolOp` from a `syn::BinOp`.
    fn from_bin_op(bin_op: &BinOp) -> Option<Self> {
        match bin_op {
            BinOp::And(_) => Some(Self::And),
            BinOp::Or(_) => Some(Self::Or),
            _ => None,
        }
    }

    /// Compares this `LogBoolOp` with the previous one and returns a complexity
    /// index.
    fn eval_based_on_prev(self, prev: Option<Self>) -> Index {
        match (prev, self) {
            (Some(prev), current) => {
                if prev == current {
                    Index::zero()
                } else {
                    Index::one()
                }
            }
            (None, _) => Index::one(),
        }
    }
}

/////////////////////////////////////////////////////////////////////////
// State type
/////////////////////////////////////////////////////////////////////////

/// Represents the current state during parsing. We use this type to track the
/// nesting level and the previous logical boolean operator.
#[derive(Debug, Default, Clone, Copy)]
struct State {
    /// The nesting level.
    nesting: u32,
    /// The previous logical boolean operator.
    log_bool_op: Option<LogBoolOp>,
}

impl State {
    /// Create a new `State` with an extra level of nesting.
    fn increase_nesting(self) -> Self {
        Self {
            nesting: self.nesting + 1,
            log_bool_op: self.log_bool_op,
        }
    }
}

/////////////////////////////////////////////////////////////////////////
// Evaluation functions
/////////////////////////////////////////////////////////////////////////

/// Returns the complexity of a `syn::Block`.
fn eval_block(block: &Block, state: State) -> Index {
    block
        .stmts
        .iter()
        .map(|e| eval_stmt(e, state))
        .sum::<Index>()
}

/// Returns the complexity of a `syn::Stmt`.
fn eval_stmt(stmt: &Stmt, state: State) -> Index {
    match stmt {
        Stmt::Local(Local {
            init: Some((_, expr)),
            ..
        }) => eval_expr(expr, state),
        Stmt::Local(Local { init: None, .. }) => Index::zero(),
        Stmt::Item(item) => eval_item(item, state),
        Stmt::Expr(expr) | Stmt::Semi(expr, _) => eval_expr(expr, state),
    }
}

/// Returns the complexity of a `syn::Item`.
fn eval_item(item: &Item, state: State) -> Index {
    match item {
        Item::Const(ItemConst { expr, .. }) => eval_expr(expr, state),
        Item::Static(ItemStatic { expr, .. }) => eval_expr(expr, state),
        _ => Index::zero(),
    }
}

/// Returns the complexity of a `syn::ExprUnary`.
///
/// This function also updates the previous logical boolean operator if it is
/// `!`.
fn eval_expr_unary(expr_unary: &ExprUnary, mut state: State) -> Index {
    let ExprUnary { op, expr, .. } = expr_unary;
    if let Some(current) = LogBoolOp::from_un_op(*op) {
        state.log_bool_op = Some(current);
    }
    eval_expr(expr, state)
}

/// Returns the complexity of a `syn::ExprBinary`.
///
/// This function handles logical boolean operators `&&` and `||` by doing the
/// following:
/// - If the operator is the different then add one to the complexity.
/// - Update the previous logical boolean operator.
fn eval_expr_binary(expr_binary: &ExprBinary, mut state: State) -> Index {
    let ExprBinary {
        left, op, right, ..
    } = expr_binary;
    let index = match LogBoolOp::from_bin_op(op) {
        Some(current) => {
            let index = current.eval_based_on_prev(state.log_bool_op);
            state.log_bool_op = Some(current);
            index
        }
        None => Index::zero(),
    };
    index + eval_expr(left, state) + eval_expr(right, state)
}

/// Returns the complexity of a `syn::ExprRange`.
fn eval_expr_range(expr_range: &ExprRange, state: State) -> Index {
    let ExprRange { from, to, .. } = expr_range;
    from.as_ref()
        .map(|e| eval_expr(e, state))
        .unwrap_or_else(Index::zero)
        + to.as_ref()
            .map(|e| eval_expr(e, state))
            .unwrap_or_else(Index::zero)
}

/// Returns the complexity of a `syn::ExprIf`.
fn eval_expr_if(expr_if: &ExprIf, state: State) -> Index {
    let ExprIf {
        cond,
        then_branch,
        else_branch,
        ..
    } = expr_if;
    Index::with_context(state)
        + eval_expr(cond, state)
        + eval_block(then_branch, state.increase_nesting())
        + else_branch
            .as_ref()
            .map(|(_, expr)| Index::one() + eval_expr(&expr, state.increase_nesting()))
            .unwrap_or_else(Index::zero)
}

/// Returns the complexity of a `syn::ExprMatch`.
fn eval_expr_match(expr_match: &ExprMatch, state: State) -> Index {
    let ExprMatch { expr, arms, .. } = expr_match;
    Index::with_context(state)
        + eval_expr(expr, state)
        + arms
            .iter()
            .map(|arm| {
                arm.guard
                    .as_ref()
                    .map(|(_, expr)| eval_expr(&expr, state))
                    .unwrap_or_else(Index::zero)
                    + eval_expr(&arm.body, state.increase_nesting())
            })
            .sum::<Index>()
}

/// Returns the complexity of a `syn::ExprForLoop`.
fn eval_expr_for_loop(expr_for_loop: &ExprForLoop, state: State) -> Index {
    let ExprForLoop { expr, body, .. } = expr_for_loop;
    Index::with_context(state) + eval_expr(expr, state) + eval_block(body, state.increase_nesting())
}

/// Returns the complexity of a `syn::ExprWhile`.
fn eval_expr_while(expr_while: &ExprWhile, state: State) -> Index {
    let ExprWhile { cond, body, .. } = expr_while;
    Index::with_context(state) + eval_expr(cond, state) + eval_block(body, state.increase_nesting())
}

/// Returns the complexity of a `syn::ExprStruct`.
fn eval_expr_struct(expr_struct: &ExprStruct, state: State) -> Index {
    let ExprStruct { fields, rest, .. } = expr_struct;
    fields
        .iter()
        .map(|v| eval_expr(&v.expr, state))
        .sum::<Index>()
        + rest
            .as_ref()
            .map(|e| eval_expr(e, state))
            .unwrap_or_else(Index::zero)
}

/// Returns the complexity of a `syn::ExprCall`.
fn eval_expr_call(expr_call: &ExprCall, state: State) -> Index {
    let ExprCall { func, args, .. } = expr_call;
    eval_expr(func, state) + args.iter().map(|a| eval_expr(a, state)).sum::<Index>()
}

/// Returns the complexity of a `syn::ExprMethodCall`.
fn eval_expr_method_call(expr_method_call: &ExprMethodCall, state: State) -> Index {
    let ExprMethodCall { receiver, args, .. } = expr_method_call;
    eval_expr(receiver, state) + args.iter().map(|a| eval_expr(a, state)).sum::<Index>()
}

/// Returns the complexity of a `syn::Expr`.
///
/// This function contains most of the logic for calculating cognitive
/// complexity. Expressions that create nesting increase the complexity and
/// expressions that increase the branching increasing the complexity.
fn eval_expr(expr: &Expr, state: State) -> Index {
    match expr {
        // Expressions that map to multiple expressions.
        // --------------------------------------------
        Expr::Array(ExprArray { elems, .. }) | Expr::Tuple(ExprTuple { elems, .. }) => {
            elems.iter().map(|e| eval_expr(e, state)).sum()
        }

        // Unary and binary operators.
        // ---------------------------
        // These are handled specially because of logical boolean operator complexity.
        Expr::Unary(expr_unary) => eval_expr_unary(expr_unary, state),
        Expr::Binary(expr_binary) => eval_expr_binary(expr_binary, state),

        // Expressions that have a left and right part.
        // --------------------------------------------
        Expr::Assign(ExprAssign { left, right, .. })
        | Expr::AssignOp(ExprAssignOp { left, right, .. })
        | Expr::Index(ExprIndex {
            expr: left,
            index: right,
            ..
        })
        | Expr::Repeat(ExprRepeat {
            expr: left,
            len: right,
            ..
        }) => eval_expr(left, state) + eval_expr(right, state),

        Expr::Range(expr_range) => eval_expr_range(expr_range, state),

        // Expressions that create a nested block like `async { .. }`.
        // -----------------------------------------------------------
        Expr::Async(ExprAsync { block, .. })
        | Expr::Block(ExprBlock { block, .. })
        | Expr::Loop(ExprLoop { body: block, .. })
        | Expr::TryBlock(ExprTryBlock { block, .. })
        | Expr::Unsafe(ExprUnsafe { block, .. }) => eval_block(block, state.increase_nesting()),
        Expr::ForLoop(expr_for_loop) => eval_expr_for_loop(expr_for_loop, state),
        Expr::While(expr_while) => eval_expr_while(expr_while, state),

        // Expressions that do not not nest any further, and do not contribute to complexity.
        // ----------------------------------------------------------------------------------
        Expr::Lit(_) | Expr::Path(_) => Index::zero(),

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
        | Expr::Yield(ExprYield {
            expr: Some(expr), ..
        }) => eval_expr(expr, state),

        // Expressions that introduce branching.
        // -------------------------------------
        Expr::If(expr_if) => eval_expr_if(expr_if, state),
        Expr::Match(expr_match) => eval_expr_match(expr_match, state),
        Expr::Continue(_) | Expr::Break(_) => Index::one(),

        // Expressions that call functions / construct types.
        // --------------------------------------------------
        Expr::Struct(expr_struct) => eval_expr_struct(expr_struct, state),
        Expr::Call(expr_call) => eval_expr_call(expr_call, state),
        Expr::MethodCall(expr_method_call) => eval_expr_method_call(expr_method_call, state),
        // FIXME: should we attempt to parse macro the tokens into something that we can calculate
        // the complexity for?
        Expr::Macro(_) => Index::zero(),

        // `Expr` is non-exhaustive, so this has to be here. But we should have handled everything.
        _ => Index::zero(),
    }
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

    #[test]
    fn logical_boolean_operators_same() {
        let expr: Expr = parse_quote! { x && y };
        assert_eq!(expr.complexity(), 1);
        let expr: Expr = parse_quote! { x && y && z };
        assert_eq!(expr.complexity(), 1);
        let expr: Expr = parse_quote! { w && x && y && z };
        assert_eq!(expr.complexity(), 1);
        let expr: Expr = parse_quote! { x || y };
        assert_eq!(expr.complexity(), 1);
        let expr: Expr = parse_quote! { x || y || z };
        assert_eq!(expr.complexity(), 1);
        let expr: Expr = parse_quote! { w || x || y || z };
        assert_eq!(expr.complexity(), 1);
    }

    #[test]
    fn logical_boolean_operators_changing() {
        let expr: Expr = parse_quote! { w && x || y || z };
        assert_eq!(expr.complexity(), 2);
        let expr: Expr = parse_quote! { w && x && y || z };
        assert_eq!(expr.complexity(), 2);
        let expr: Expr = parse_quote! { w && x || y && z };
        assert_eq!(expr.complexity(), 3);
    }

    #[test]
    fn logical_boolean_operators_not_operator() {
        let expr: Expr = parse_quote! { !a && !b };
        assert_eq!(expr.complexity(), 1);
        let expr: Expr = parse_quote! { a && !(b && c) };
        assert_eq!(expr.complexity(), 2);
        let expr: Expr = parse_quote! { !(a || b) && !(c || d) };
        assert_eq!(expr.complexity(), 3);
    }
}
