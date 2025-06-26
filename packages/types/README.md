# Type Theory

## Typing Rules

### $\Pi$ Type Introduction

A $\Pi \: Type$ is not something that is analyzed in standard dependent type theory, so doesn't have an elimination rule.

$$
\dfrac{
    \Gamma \vdash A : Type_i \qquad \Gamma, x : A \vdash B : Type_i
} {
    \Gamma \vdash \Pi x : A . \: B : Type_i
}
[\Pi \: Type]
$$

We don't use indexed type universes, as this isn't a theorem proving language.

### Abstraction

The introduction rule for $\lambda$ abstractions:

$$
\dfrac{
    \Gamma, x : A \vdash e : B
} {
    \Gamma \vdash \lambda x : A . \: e : \Pi x : A . \: B
}
[Abstraction]
$$

### Application

This is the elimination rule for $\lambda$ abstraction:

$$
\dfrac{
    \Gamma \vdash f : \Pi x : A . \: B \qquad \Gamma \vdash a : A
} {
    \Gamma \vdash f \: a : B[a/x]
}
[Application]
$$

The term $B[a/x]$ means replace $x$ with $a$ in $B$ (which is necessary as $B$ depends on $x$ in the premise).

### Let

#### Introduction

$$
\dfrac{
    \Gamma \vdash a : A \qquad \Gamma, x : A \vdash e : B
}{
    \Gamma \vdash \text{let } x : A = a \text{ in } e : B[a/x]
}[Let]
$$

#### Elimination

$$
\text{let } x : A = a \text{ in } e \equiv e[a/x] [Computation]
$$
