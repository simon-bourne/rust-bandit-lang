# Type Inference

## Rank N Types

Examples:

```bandit
∀f b c r. (∀a. a -> r) → b → c → r
∀f b c r. (∀a. f where FnOnce f a r) → b → c → r
```

`f` is `Sized` by default.

## Let Should Not be Generalized

TODO: Description and reference.

## Impredicativity

We just live with the limitations, rather than implement something like Quick Look.
TODO: References
