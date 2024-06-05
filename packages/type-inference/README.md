# Type Inference

## Rank N Types

Examples:

```bandit
∀f b c r. (∀a. a -> r) → b → c → r
∀f b c r. (∀a. f where FnOnce f a r) → b → c → r
```

`f` is `Sized` by default.

## [Let Should not be Generalized]

[Discussion](http://lambda-the-ultimate.org/node/3853).

## Impredicativity

We just live with the limitations, rather than implement something like [Quick Look](https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/).

[Let Should not be Generalized]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf?from=https://research.microsoft.com/en-us/um/people/simonpj/papers/constraints/let-gen.pdf&type=exact
