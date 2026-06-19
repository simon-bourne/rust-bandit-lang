# Plan

## Errors

- id's can be tied back to source.
- id's indicate whether they were originally known.
- Unifications form a graph.
- Each unification has a reason (e.g. because the parents were unified, or because it's in a function application).
- Each unification reason has a complexity cost.

### Reporting

- Errors happen when the unification of 2 terms fails.
- Trace each term through the unification graph, finding the cheapest path until we find a term that is originally known.

### Implementation

Get id into shape where it:

* knows if it was originally known.
* has a slot id (from context).

---

- Static and dynamic versions of lambda and let. Static lambdas are required to properly implement `id`
- Static parameter inference
- Matchers
- Friendly errors
- Traits
