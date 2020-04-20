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


Based on [Cognitive Complexity][pdf] by G. Ann Campbell.

[pdf]: https://www.sonarsource.com/docs/CognitiveComplexity.pdf

## Getting started

Add `complexity` to your `Cargo.toml`.

```
[dependencies]
complexity = "0.1"
syn = "1"
```

You'll need to import the [`Complexity`] trait, and probably some parts of
[`syn`].

```rust
use complexity::Complexity;
use syn::{ItemFn, parse_str};
```

Loops and branching increment the complexity by one each. Additionally,
`continue` and `break` statements increment the complexity by one.

```rust
let code = r#"
fn sum_of_primes(max: u64) -> u64 {
    let mut total = 0;
    'outer: for i in 1..=max {
        for j in 2..i {
            if i % j == 0 {
                continue 'outer;
            }
        }
        total += i;
    }
}"#;

let func: ItemFn = parse_str(code)?;
assert_eq!(func.complexity(), 7);
```

However, a `match` only increases the complexity by one no matter how many
branches there are.

```rust
let code = r#"
fn get_words(number: u64) -> &'static str {
    match number {
        1 => "one",
        2 => "a couple",
        3 => "a few",
        _ => "lots",
    }
}"#;

let func: ItemFn = parse_str(code)?;
assert_eq!(func.complexity(), 1);
```

[`Complexity`]: https://docs.rs/complexity/0.1/trait.Complexity.html
[`syn`]: https://docs.rs/syn/1

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.
