# Plan

- Add value constructors to context
- In `Context::infer_types`, call `Term::type_constructor` (should this be renamed? It's not creating a type contructore, just enforcing constraints)
- Matchers
- Friendly errors
- Traits
