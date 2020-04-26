# Releases

## 0.2.0

*Release on April 26th, 2020*

- [`.complexity()` now returns a `u32` instead of a `u64`.](695cf51)

- [Correctly calculate complexity for sequences of logical boolean operators.](f411aca)

  Multiple boolean operators now only increase the complexity when they change.
  For example:

  ```rust
  let expr: Expr = parse_quote! { x && y && z };
  assert_eq!(expr.complexity(), 1);

  let expr: Expr = parse_quote! { x && y || z };
  assert_eq!(expr.complexity(), 2);
  ```

[695cf51]: https://github.com/rossmacarthur/complexity/commit/695cf51d70db5d23dc257f1b0f4070edb984345a
[f411aca]: https://github.com/rossmacarthur/complexity/commit/f411acace56fba5c94f1d2187223ffb9af56be28

## 0.1.1

*Released on April 25th, 2020*

- [Properly take into account nesting level.](86429fc)

  Previously this was not implemented correctly and multiple nested structures
  would not accurately contribute to the complexity.

- [Implement `Complexity` for `syn::Expr`.](f9aa737)

  This means you can calculate the complexity of arbitrary expressions:

  ```rust
  let expr: Expr = parse_quote! {
      if something {
          do_something()
      }
  };
  assert_eq!(expr.complexity(), 1);
  ```

- [Add `lint-files` example.](fc0417a)

  See [examples/lint-files.rs](examples/lint-files.rs).

[86429fc]: https://github.com/rossmacarthur/complexity/commit/86429fc57188cd65a87da659ee9101e96594e1f2
[f9aa737]: https://github.com/rossmacarthur/complexity/commit/f9aa737586797744fd14f84f1b43197b42b4639a
[fc0417a]: https://github.com/rossmacarthur/complexity/commit/fc0417ab693154ba57b32de0a9013e92b53ebb72

## 0.1.0

*Released on April 20th, 2020*

- First release
