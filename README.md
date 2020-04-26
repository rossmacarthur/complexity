<h1 align="center">complexity</h1>
<div align="center">
  <strong>Calculate cognitive complexity of Rust code.</strong>
</div>
<br />
<div align="center">
  <a href="https://crates.io/crates/complexity">
    <img src="https://img.shields.io/crates/v/complexity.svg" alt="Crates.io version" />
  </a>
  <a href="https://docs.rs/complexity">
    <img src="https://docs.rs/complexity/badge.svg" alt="Documentation" />
  </a>
  <a href="https://github.com/rossmacarthur/complexity/actions?query=workflow%3Abuild">
    <img src="https://img.shields.io/github/workflow/status/rossmacarthur/complexity/build/master" alt="Build status" />
  </a>
</div>
<br>

Based on [Cognitive Complexity][pdf] by G. Ann Campbell.

## Getting started

Add `complexity` to your `Cargo.toml`.

```
[dependencies]
complexity = "0.1"
syn = "1"
```

You'll need to bring the [`Complexity`] trait into scope, and probably some
things from [`syn`].

```rust
use complexity::Complexity;
use syn::{Expr, parse_quote};
```

Complexity of expressions and other [`syn`] types is as simple as calling
[`.complexity()`] on an instance of that type.

```rust
let expr: Expr = parse_quote! {
    for element in iterable { // +1
        if something {        // +2 (nesting = 1)
            do_something();
        }
    }
};
assert_eq!(expr.complexity(), 3);
```

## Examples

The implementation of cognitive complexity in this crate is heavily based on
[Cognitive Complexity][pdf] by G. Ann Campbell. And reading it would be
beneficial to understanding how the complexity index is calculated.

Loops and structures that introduce branching increment the complexity by one
each. Some syntax structures introduce a "nesting" level which increases some
expressions complexity by that nesting level in addition to their regular
increment. In the example below we see how two nested loops and an if statement
can produce quite a high complexity of **7**.

```rust
use complexity::Complexity;
use syn::{ItemFn, parse_quote};

let func: ItemFn = parse_quote! {
    fn sum_of_primes(max: u64) -> u64 {
        let mut total = 0;
        'outer: for i in 1..=max {   // +1
            for j in 2..i {          // +2 (nesting = 1)
                if i % j == 0 {      // +3 (nesting = 2)
                    continue 'outer; // +1
                }
            }
            total += i;
        }
    }
};
assert_eq!(func.complexity(), 7);
```

But some structures are rewarded. Particularly a `match` statement, which only
increases the complexity by one no matter how many branches there are. (It does
increase the nesting level though.) In the example below we see how even though
there are a lot of branches in the code (which would contribute a lot to a more
traditional *cylomatic complexity* measurement), the complexity is quite low at
**1**.

```rust
use complexity::Complexity;
use syn::{ItemFn, parse_quote};

let func: ItemFn = parse_quote! {
    fn get_words(number: u64) -> &str {
        match number {       // +1
            1 => "one",
            2 => "a couple",
            3 => "a few",
            _ => "lots",
        }
    }
};
assert_eq!(func.complexity(), 1);
```

An example is provided to calculate and nicely print out the cognitive
complexity of each function and method in an entire Rust file. See
[examples/lint-files.rs](examples/lint-files.rs). You can run it on Rust files
like this:

```sh
cargo run --example lint-files -- src/
```

[pdf]: https://www.sonarsource.com/docs/CognitiveComplexity.pdf
[`Complexity`]: https://docs.rs/complexity/0.1/complexity/trait.Complexity.html
[`.complexity()`]: https://docs.rs/complexity/0.1/complexity/trait.Complexity.html#tymethod.complexity
[`syn`]: https://docs.rs/syn/1

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.
