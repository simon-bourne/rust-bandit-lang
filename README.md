# Bandit

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/simon-bourne/rust-bandit-lang)

A Rust-like experimental programming language for me to play around with:

- Parameterized modules/associated traits
- Higher kinded types
- Effects
- Dependent types
- Linear types
- Non subtyping lifetimes. For example: `f(x: &T, y: &T) -> &T where 'f = min('x, 'y)` instead of `f<'a>(x: &'a T, y: &'a T) -> &'a T`
- Deferred borrows
- Macros with quoting/unquoting, and arbitrary arguments: `my-macro {some tokens} "a compile time string argument"`
- Tuples as syntactic sugar for pairs, so `a, b, c` is syntactic sugar for `a, (b, c)`
- Alternative `do` notation. Can something like Rust's `?` operator demarcate computations to be Monadically bound?
