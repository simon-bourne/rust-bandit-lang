# Language

## Benefits Over Rust

- Higher kinded types.
- Effects.
- Cleaner syntax.
- Marker polymorphism.
- Generalized tuples.
- Parameterized modules
- GADTs.
- Macro hygiene.

## Layout

Blocks start with the keywords `do`, `then`, `where`, `match` and `case`. If block start keyword is at the start of a line, it should be indented by the same amount as the block header statement:

```bandit
block header
do
    a statement
    another statement

    nested block header then
        a statement
        etc
```

If anything appears on the same line after the block keyword, it is a single line block:

```bandit
start of block do start of another block do statement
```

Omit the block start keyword for empty statements. We don't use `pass`, like python, as we use keywords to indicate the start of a block, instead of `:`, so it's visually obvious when a block is empty.

```bandit
trait EmptyTrait

type Void
```

### Comments

Single line comments start with a `#`. This allows us to ignore the shebang in unix scripts.

Multiline comments are not supported because:

- comments are unstructured text that may contain any "end of comment" marker. For example, a code snippet may contain a string with the "end of comment" marker. This isn't such a problem with layout languages as each line in a block is prefixed by the same indentation.
- if a comment starts a new layout block, it really needs to start the line. That means it can't be used to comment part of an expression.
- single line comments give a clear separation between code and comments, even without syntax highlighting.

#### Doc Comments

Whether a comment is a doc comment depends on where it is placed. If it's before a public item, it's a doc comment. If you want to document the implementation of a public function or type for example, put the comment inside the implementation block. If you want an implementation note for a module or group of functions, put it in a private module.

Doc comments are formatted using a new markup language:

- `{use my_module(a, b)}` will import `A` and `B` from `my_module`.
- `{my_expression}` will evaluate `my_expression`. For example `{superscript "superscript text"}`. The return type should be `DocFragment`.
- `[my text]` will call `bracketed "my text"`, which should return a `DocFragment`. `[my text][my other text]` will call `double_bracketed "my text" "my other text"`. These can be used for:
  - Named links
  - Checkboxes (`[ ]` and `[x]`)
  - Styled text, for example italic could be `[+italic text]`. The first non alphanumeric characters define the style:
    - `*` emphasis (italic)
    - `**`strong (bold)
    - `***` strong emphasis (bold italic)
    - `=` for highlighted
    - `+` for *ins* (underline)
    - `-` for *del* (strikethrough)
    - `^` for superscript
    - `~` for subscript
    - `"` or `'` for smart quotes
    - `:emoji_name` for an emoji
    - `.class` to apply `<span class=".class">...</span>`
    - `_` for a footnote. `DocFragment` would need to support footnotes.
- `[my text](additional text)` will call `link "my text" "additional text"`.
- `body_of my_function` is a built in function that produces a listing for the body of `my_function`.
- There are shortcuts to some functions:
  - `# My Heading`, `## My Heading` to call `heading 1` and `heading 2` with `"My Heading"`. The text is read until the end of the paragraph.
  - `---` on a line by itself calls `horizontal_rule`.
  - `` `my code` `` to call `generic_code "my_code"`. Specific language code, for example Rust, should be called with `{rust "my code"}`.
  - Lists: top level lists must be surrounded by blank lines to stop split lines being confused for lists. They start with `-`, `1.` or `:`. for unordered, ordered and definition lists respectively. Soft line breaks in the paragraph must be indented by the same as the list item. Extra paragraphs for a list item are indented once from the main list item marker.

    Sub lists are indented once from the parent list item marker, and if the parent list item is more than 1 paragraph, they must be preceded by a blank line.

    The first paragraph in a definition list item is the "term". Any subsequent paragraphs are the "definitions".

  - Tables use [djot](https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html#pipe-table) syntax.
  - Any group of lines starting with `>` is a block quote.
- Any single character can be escaped with a `\`.
- `...` is translated to an elipsis.
- `--` is translated into an en-dash.
- `---` is translated into an em-dash.
- TODO: Images

Things like math can be implemented with a `math` function. It could take an expression tree which can be built from native expressions. e.g.`{math (var "x" + 1)}`.

Documents are structured as trees. A tree node can be wrapped by putting `@{expression}` before it. This applies `expression` to the (opaque) rendered block.

TODO: Checkout [Djot](https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html) syntax.

#### Doctests

There's no such thing as a doctest, just a unit test. The body of a function can be included in a comment. If you want a function definition included, make the function local to the function body.

## `case` vs `match`

Using `match`, an expression is bound to the patterns. Using `case` defines a function that takes the pattern expressions as arguments.

## Syntax

The syntax is generally Haskell-like, but strict. To make a type into a reference, just add a lifetime. Lifetimes cannot be elided.

## Functions

```bandit
my_function1 x = stuff

my_function2 : a -> b case
    x then stuff

my_function3 : a -> b =
    \x: stuff

my_function4 x = \y: stuff

my_function5 : forall a b. a -> b case
    x then stuff

my_function6 : forall (a : Type) (b : Type). a -> b case
    some_pattern if condition then stuff
    another_pattern then other_stuff
    else more_stuff

my_function7 : Ord a => a 'b -> a 'b -> a 'b case
    x y then
        if x > y then x
        else y

my_function8 : Option a -> Option a -> Option a case
    (Some x) _ then x
    _ (Some y) then y
    _ _ then None

my_function9 : Option a -> Option a -> Option a case
    (Some x) _ then x
    _ (Some y) then y
    _ _ then None

lifetime_quantification : forall ('a : Lifetime). Int 'a -> Int 'a

multiplicity : Int 'a 1 -> Int 'a 1

multiplicity_polymorphism :  Int 'a n ->  Int 'a n
```

## Contexts

Rust:

```rust
fn f<A>(x: impl X)
where
    A: X + Y + 'static,
    A::B: X + Z<Int>,
{...}
```

Bandit:

```bandit
f 
    : (X a, Y a, 'static a, c ~ a.b, X c, Z c Int)
    => (x : X) : ()
do
    ...
```

## Type Application

Like [Ghc's TypeApplications](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html). Unlike Ghc, if an explicit `forall` is not specified, the order of variables is strictly the order they appear in the type. Type application is not supported on inferred types. Use `infer` for a type hole.

```bandit
f : a -> a -> a

g = f @ Int
```

We don't allow types (or normal parameters) to be specified via named parameters because:

- handling the remaining parameters become more complex.
- the name of parameters becomes part of the public interface.

## Rank N Types

```bandit
rank2 : (forall a. a -> b) -> c -> d
```

We need to [defunctionalize](https://en.wikipedia.org/wiki/Defunctionalization) this to lower it to Rust.

## Constants

```bandit
my_constant1 = 42
my_constant1 : Int = 42
```

## Literals

Multiline string literals begin with 3 or more `"`s, and end with the same number. They can't be empty, as that would make parsing difficult. For example:

```python
"""
line1
line2
"""

""""
line 1
line 2
""""
```

## Closures

```bandit
\x y z = stuff
\(x : Int) (y : Int) : Int = stuff
\x y z do
    stuff
    more_stuff
```

There are no zero argument functions. `\do` is short for `\() do`.

Some control flow examples:

```bandit
xs.for \x do
    stuff
    more_stuff

xs.for \x = stuff
```

## Data Types

### Tuples

There are only 2 types of tuple, `()`, and `Pair x y`. `(x, y, z)` is syntactic sugar for the nested pairs `(x, (y, (z, ())))`. This allows traits to be written to consume the tuple. For example:

```bandit
trait Apply f a r where
    apply : f -> a -> r
  
impl Apply f_tail tuple_tail result => Apply (arg -> f_tail) (arg, tuple_tail) result where
    apply f (fst, snd) = apply (f fst) snd

impl Apply result () result where
  apply x () = x

f : Int -> Int -> Int = (+)

g : Int = apply f (1 : Int, 2 : Int)
```

### Sums

```bandit
type MySum =
    MyEmptyVariant |
    MyVariant U32 |
    MySumOfProduct{field1 : U32, field2 : U32}

type MyGenericSum a b = Single a | Pair{first : a, second : b}

type Term a where
    Empty : Self ()
    Literal : a -> Self a
    Equal : Compare b => b -> b -> Self Bool
    Pair : {first : f, second : s} -> Self (f, s)
    ExplicitlyQuantifiedPair : forall f s. {first : f, second : s} -> Self (f, s)
```

Type constructors are namespaced under their type. For example, `MySum.EmptyVariant`.

GADTs are not very easy to map to Rust. For example, the `Equal` variant needs to be expanded to include a variant for each type sent to `Equal` (or alternatively, erase the type via a boxed `dyn` trait).

With GADTs, we can use the more specific result type in a pattern match:

```bandit
eval : Term a -> a case
    Empty then ()
    Literal i then i
    Equal i j then i == j
    Pair x then (x.first, x.second)
    ExplicitlyQuantifiedPair {first, second} then (f, s)
```

This is where things diverge from Haskell. Record syntax is not just sugar. So `x` in `Pair x` above is the entire record, which can be accessed with `.` notation.

### Products

Products are just sums with only 1 variant. It's suggested to use the name `New`, unless some other name makes sense.

For example:

```bandit
type MyUnit = New
type MyType = New U32
type MyProduct = New{field1 : U32, field2 : U32}
type MyGenericProduct a = New{field1 : U32, field2 : a}
```

### Records

Differently to Haskell, we have a class with field accessors for each record. Duplicate names across traits are allowed and disambiguated by the typechecker.

### Higher Kinded Types

```bandit
type Wrapper (container : Type -> Type) (element : Type) = New (container element)

hkt
    : forall (container : Type -> Type) (element : Type).
    MyConstraint container
    => container element -> ()
```

## Dot Notation

`.` is used for resolving scopes, not function composition. Function composition is:

```bandit
f ~< g
```

## Relevant Types

Types have a `Drop` marker to describe allowed behaviour around dropping:

- `Anywhere`: Values can be freely dropped.
- `OnError`: Values can be discarded when effects terminate, but not when they go out of scope.
- `Never`: Values cannot be dropped. They must be destructured or moved before they go out of scope.

## Traits

Traits use the Rust coherency rules.

### Disambiguating Trait Members

They also don't do an ambiguity check like Haskell.

```bandit
trait MyTrait t where
    alias MyType

    f : Int
```

Both `MyType` and `f` are ambiguous, because `MyType` and `f` don't include the anything from the instance head. Haskell forbids this, but we allow it. Trait items can be disambiguated with:

- `variable.method` where `method` has a first argument with the same type as `variable`.
- `TraitName a b c.item` where `item` is any trait item.
- `TypeName a b c.item` where the type is the first type in the `impl` head.

```bandit
trait A a where
    alias R
    f : Self.R

trait B a where
    f : Int

type X

impl A X where
    alias R = Int
    f = 1

impl B X where
    f = 2

g : Int = A X.f

data Y a = New a

impl A (Y a) where
    alias R = Int
    f = 1

impl B (Y a) where
    f = 1

data Z = New Int

impl A Z where
    alias R = Int
    f = 1

h : Y t -> A.R case
    _ then A (Y t).f

j : Z -> Z.R case
    _ then Z.f
```

`h` is equivalent to the Rust code:

```rust
fn h<T>(_: Y<T>) -> A::R {
    <Y<T> as A>::f()
}
```

### `Fn` Traits

The act of calling a function either consumes it (for owned `FnOnce` values), uniquely uses the reference (for `FnMulti` values) or uses the shared reference (for `Fn` values). Type signatures are desugared depending on whether their args are references, unique references, or owned. Currying gives back something based on it's current `Fn` + whether it's newly bound argument is a shared ref (`Fn`), unique ref (`FnMut`) or owned (`FnOnce`).

```bandit
f : a -> b -> c
f1 : Fn(a -> FnOnce(b -> c))

g : a 'x -> b -> c
g1 : Fn(a 'x -> Fn(b -> c))

h : a 'x 1 -> b -> c
h1 : Fn(a 'x 1 -> FnMut(b -> c))
```

Functions by default are desugared to `Fn`:

```bandit
use_fn_multi : (a -> b) -> a -> b
use_fn_multi1 : Fn (a -> b) -> a -> b

use_fn_mut : FnMut (a -> b) -> a -> b
use_fn_once : FnOnce (a -> b) -> a -> b
```

The default trait to return is `Fn`.

```bandit
make_fn_multi : a -> b
make_fn_mut : FnMut (a -> b)
make_fn_once : FnOnce (a -> b)
```

#### Implementing `Fn` Traits

```bandit
impl Fn MyType where
    call x = do_stuff
```

### Named Implementations

Named implementations are a bad idea because:

- They break global coherence. For example, an `insert` function on `OrderSet Int` could be called with a `Reverse` `Ord` instance and put the new element in the wrong place.
- There are issues around subtyping. What happens if we add another trait to the context of `Monoid`? Would a `Sum` implementation need to implement it? It may be incorrect to use the global `impl`.

## Modules

```bandit
module my_module
```

### Exporting Names

`public` can be put in front of any trait, type, or top level function with type signature to export the name. `public *` can be put in front of any

- type: meaning export all data constructors
- trait: meaning you can use the methods, or provide extra implementations for the trait
- alias: aliases are always transparent

### Importing Names

```bandit
use
    std (X, RenamedY = Y)
    my_module
    my_module (A, f)
        deeply_nested = my_nested_module.my_deeply_nested_module
```

This will import `std.Y` as`RenamedY`. These are all the imported names:

- `X`
- `RenamedY`
- `my_module`
- `A`
- `f`
- `deeply_nested`

### Parameteric Modules

```bandit
use
    other_module.MyTrait

module my_module : forall a b. (Eq a, MyTrait b)
...
```

`my_module` takes 2 types, `a` and `b` where `(Eq a, MyTrait b)`. Types `a` and `b` can be used in module items. All module items are distinct from other instances of this module with a different type.

Parameteric modules can be imported with:

```bandit
use
    my_module
        my_sub_module Int Int (A, f)
        my_byte_module = my_sub_module Byte Byte
        my_byte_module (A, f)
```

## Control Flow

Some control flow is implementated natively:

- because the borrow checker needs to understand it.
- otherwise the pattern matching would be unweildy.

In particular, with `if`/`else` and `match`, the borrow checker needs to understand that only one branch will be run, so it's OK to move the same thing inside each branch.

```bandit
match x where
    Some x if x > 10 then ...
    Some 10 then ...
    _: ...

if x < 10 then
    something

if x < 10 then
    something
else if x < 20 then
    something_else
else
    something_else_again

while let Some x = y do
    stuff
    more_stuff
```

## Effects

Effectful blocks are compiled to [delimited continuations]. Each continuation is delimited by an action that takes some parameters and yields a value.

- The continuation state could allocate an untagged union in which to store the action arguments and results. Actions could read and write them.
- The stack can be used for "tail resumptive" actions, but that would mean they wouldn't yield. This could be confusing where an action expects clients to be able to observe some state between continuations. Iteration, for example. Therefore it would need to be explicit.

The order of effects only matters when you're peeling them off the stack: See this Stack Overflow answer about [Monad Transformers]. In particular, the return value will change if 2 or more handlers wrap it. `WrappedByHandler2 (WrappedByHandler1 x)` if handler 1 is run first, vs `WrappedByHandler1 (WrappedByHandler2 x)` if handler 2 is run first.

```mermaid
flowchart LR
    id1[Continuation] --> id2[Action] --> id3[Continuation] --> id4[Action] --> id5[Continuation] --> id6[Return]
```

Client code is divided into continuations that take the result of an action and return the next action, like the diagram above.

Applying a handler to the continuation code will call the handlers actions inline with the client code after each continuation.

An action has serveral options:

- Call the continuation directly on the same call stack.
- Drop the continuation, which will cleanup all the objects on the continuation stack.
- Queue the continuation somewhere.
- Clone the continuation and do any of the above with each copy.

[LLVM Coroutines] may not be suitable, as Rust/Zig don't use them. [WebAssembly Effect Handlers] are another possibility.

TODO: How to label functions as copyable in a delimited continuation context?

An effect is a trait with methods of the form:

```bandit
method
    : Self 'a mut
    -> Continuation ActionReturnType output
    -> ActionArg1
    ...
    -> ActionArgN
    -> Self.Output
```

and a parent trait:

```bandit
trait Effect state output where
    alias Output;

    result : output -> Self.Output
```

Handling an effect produces a `Continuation` that can be sequentially run or interleaved with other tasks via a user defined scheduler.

```bandit
type Continuation input output

impl Continuation input output where
    # Run to completion
    resume : Self -> input -> output
    run_one : Self -> input -> Option output

type Task state output

impl Task state output where
    run_one : Self -> state 'a mut -> output
```

`handle` is a built in function that divides a function into delimited continuations and links effect action calls to effect trait methods. `::` is a cons for sets.

```bandit
handle : (() -> {e :: es} ()) -> impl e -> {es} ()
```

### Examples

#### Exceptions

Define the exception effect:

```bandit
trait Effect state output => ExceptionEffect state output where
    alias Error

    throw
        : state 'a mut
        -> Continuation String output
        -> Self.Error
        -> Self.Result

type Exception error = New (PhantomData error)

impl Effect (Exception error) output where
    alias Result = Result output error

    result = Ok

impl ExceptionEffect (Exception error) output where
    alias Error = error

    throw _self _contination = Err

try (f : () -> {Exception error :: effects} result) : {effects} result do
    (handle f).run Exception.New.mut
```

Define an error type and try/catch an exception:

```bandit
type MyError = New String

f : () -> Result Int MyError = \do
    try \do
        print "About to throw"
        throw? (MyError "Testing")
        print "Unreachable!"
        42
```

#### IO

An IO action will kick off the IO then pass a future back to the continuation. The future is passed to an action handler called await to handle it. The action handler must match up the future to the IO result and then pass the future output back to the continuation. This means `await?` would just be an effect action.

TODO: Write the example

#### Async

To spawn a task, an additional continuation is passed to the action.

TODO: Write the example

#### Iterator

```bandit
trait Effect state output => IteratorEffect state output where
    alias Item

    yield
        : state 'a mut
        -> Continuation () output
        -> Self.Item
        -> Self.Result

type IteratorData item = New (Option item)

impl Effect (IteratorData item) () where
    alias Result = ()

    result  = id

impl IteratorEffect (IteratorData item) where
    alias Item = item

    yield self cont i do
        self.0 = Some i
        cont.resume ()

type Generator item = New (Task (IteratorData item) ())

impl Generator item where
    new
        (f : () -> {IteratorEffect :: effects} ())
        : {effects} Self
        = (handle f).run (IteratorData.New @ item None).mut

impl Iterator (Generator item) where
    alias Item = item

    next self do
        let mut data = IteratorData None
        self.0.run_one data.mut
        data.0
```

## Kinds

Kinds are the type of types.

```bandit
Int : Type
alias Type = Kind 0
Type : Kind 1
Kind n : Kind (n + 1)
```

## Lifetime Subtyping Constraints

```rust
fn f<'a, 'b: 'a, T>(x: &'a T, y: &'b T)
```

is equivalent to:

```bandit
f : ('a <= 'b) => t 'a -> t 'b -> ()
```

## Keywords

- `alias`
- `case`
- `do`
- `else`
- `forall`
- `if`
- `impl`
- `infer`
- `match`
- `module`
- `let`
- `loop`
- `return`
- `Self`
- `then`
- `trait`
- `type`
- `use`
- `where`
- `while`

## TODO

## Brand Types

Each time a `GhostToken` is created, we need to make sure it's brand type can't be unified with another. It would be great to allow this type to be scoped to a non-copyable value.

## Constraint Kinds

This allows constraint aliases, among other things. See GHC's [ConstraintKinds](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/constraint_kind.html) language pragma. We could just do constraint aliases as `type NewConstraint[A] = (Constraint(A), Constraint(B))`. Need to think about constraint syntax. Using `T: Constraint => Type`, `T: Constraint` and `T: Type` are ambiguous. How about `T => Constraint: Type`

## Misc

Effects (algebraic or mondaic)? i.e. Do we allow multiple resumes from the effect handler? See [Structured Asynchrony with Algebraic Effects](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/05/asynceffects-msr-tr-2017-21.pdf)

Macros
Reflection
Tail calls?
Identity functor
GC as an effect? Regional GC?
Allocation as an effect?

## Type Synonyms

`alias MyType[a, b, c] = ...`

## Macros

### Zig

[Array of struct to struct of arrays](https://github.com/ziglang/zig/commit/0808d98e10c5fea27cebf912c6296b760c2b837b)

### Hygiene

All macro code is generated in a module that is inaccessible to anything else. All public symbols are exported into the parent.

### Deriving

```bandit
pub type Term[a] = pub
    Lit(Int) where a = Int
    Pair(Term x, Term y) where a = (x, y)
    deriving (Eq, MyTraits)
```

A derive macro for `Eq` can only produce an instance for `Eq`. A trait alias can be specified, and the deriving macro must derive all the traits listed on the RHS of the alias before the `where` clause.

## Pin

A `Move` trait would have been easier to implement in Rust, but wouldn't have been backwards compatible. See [Changing the Rules of Rust](https://without.boats/blog/changing-the-rules-of-rust/).

## DataKinds

Simple values could be turned into types. Like `type(MyEnum.Variant1)`, or `type(1)`. They could implement a `Const[ValueType]` trait with a `value()` method which gives simple const generics.

## Marker Polymorphism

We want a mechanism to carry a marker trait from a generic parameter to a result. For example, we may want to take a future, and return one with the same `Send`ness. Same for `Sync` and `ScopeDrop`. Would we ever want to do this for non-marker traits?

```bandit
run[F](f: F) -> impl Future<Output = ()> + F(Send + Sync + ScopeDrop)
where
    F: Future<Output = ()>
```

Or maybe we need `AsyncSend` and plain `Async` effects (or some kind of polymorphic `Async`)? Maybe `Future` could have an extra type parameter for the thread safety, with appropriate `impl`s for `Send`?

We could implement compile time markers as a single trait with associated types. e.g:

```bandit
trait Marker {
    const Send: bool = true;
    const Move ...
}

run[F](f: F) -> impl Future<Output = ()> + Marker<Move = F::Move> 
where
    F: Future<Output = ()>
{
    ...
}
```

Then everything would implement `Marker`.

## Signals

```bandit
impl[First, Second, SigFirst, SigSecond] Signal[Item = (First, Second)] for (SigFirst, SigSecond)
where
    SigFirst: Signal<Item = First>,
    SigSecond: Signal<Item = Second>
{
    ...
}

type Pair[First, Second](first: First, second: Second);

impl[First, Second] Pair[First, Second] {
    new_signal(
        first: impl Signal[Item = First], 
        second: impl Signal[Item = Second]
    ) -> impl Signal[Item = Self] {
        ...
    }
}
```

## GhostCell

Does compile time support for `GhostCell` add anything, as it requires being inside a closure currently. Is there a generalization for tracking the lifetime of things here?

## References

Is there anything we can do about having to convert to/from refs all the time? Maybe a mutability checker + GC + GC Effect, then we can gradually degenerate to something like Rust? Check out [Mutable Value Semantics](https://www.jot.fm/issues/issue_2022_02/article2.pdf)

## Garbage Collection

Provide hooks for GC? Need to know if a thing is on the stack (directly or indirectly), or the heap. Also need to know when it moves. Is it possible to know if it's on the stack? A function could be called on a mutable ref, and it's unknown if that ref is on the stack. We'd also need to be able to collect all the `Gc` things that live inside an object. That would infect the language quite substantially. A vector, for example, would need to implement `Collectible`. Maybe it's not too bad.

### Borrowing

Effects interact with borrowing, as the borrow checker needs to know if an effect terminates execution or may continue.

### Exceptions

Rather than returning a union of errors, multiple exception effects could be combined and handled individually. Of course, an expection with a normal union type can still be used.

## Type Inference

[Problems with type inference is a problem introduced with polymorphism](https://news.ycombinator.com/item?id=20717635). You can't infer the type of `read` for example.

## Single Method Traits

Traits with single methods can just inherit `Fn` traits:

```rust
trait MyTrait: FnOnce() -> U32 {}
```

## Const Generics

TODO: Should we just use `type(1)` with a constraint of `Value[T]` etc?
TODO: Which value types should be allowed? We need to be able to equality match for instance selection. Should we do a structural equality match, or use the `Eq` instance?

The type of compile time arguments are assumed `Type`. Value types are allowed.

```bandit
f[T, U, V, Value: U32]() {

}

f[T, U, V, 1]();
```

## Scoped Types

It might be useful to have a type that only exists while a variable is in scope. This could be used for ghost cell.

## Build System

- Module: a collection of source files
- Compilation unit: a collection of modules, and a lightweight build file to specify dependencies
- Package: A collection of compilation units + a main compilation unit
- Executable: Build configuration (platform, features etc) is specified using types/traits.

## Links

- [Syntax and Semantics of Quantitative Type Theory](https://bentnib.org/quantitative-type-theory.pdf)
- [Interesting article about `PhantomData` and subtyping](http://troubles.md/why-phantomdata/)
- [Less Painful Linear Types](http://aidancully.blogspot.com/2021/12/less-painful-linear-types.html)
- [Idris Named Instances](https://idris2.readthedocs.io/en/latest/tutorial/interfaces.html#named-implementations).
- [Exotic Programming Ideas: Part 1 (Module Systems)](https://www.stephendiehl.com/posts/exotic01.html)
- [Understanding and Evolving the ML Module System](https://people.mpi-sws.org/~dreyer/thesis/main.pdf)
- [Asyncify](https://kripken.github.io/talks/2022/asyncify.html#/)

### Interesting Languages

- [Frank](https://arxiv.org/pdf/1611.09259.pdf)
- [Koka](https://koka-lang.github.io/)

### Libraries

- [eff (Haskell)](https://hasura.github.io/eff/Control-Effect.html)

### Haskell Extensions

- [BlockArguments]
- [Kind Signatures](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/kind_signatures.html)
- [Explicit Universal Quantification](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html)
- [ConstraintKinds](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/constraint_kind.html)
- [Existentially Quantified Data Constructors](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html)

[BlockArguments]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html?highlight=blockarguments
[Monad Transformers]: https://stackoverflow.com/a/5076096
[delimited continuations]: https://www.youtube.com/watch?v=TE48LsgVlIU
[WebAssembly Effect Handlers]: https://wasmfx.dev/
[LLVM Coroutines]: https://llvm.org/docs/Coroutines.html
