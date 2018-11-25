PureScript JSON Schema
======================
A proof of concept project generating JSON schema from PureScript types. Not
ready for primetime yet.

Usage
-----
```purescript
type User =
  { name :: String
  , age :: Int
  }

jsonSchema (definition :: Definition User)
-- {"type":"object","required":["age","name"],"properties":{"age":{"type":"integer"},"name":{"type":"string"}}}
```

Roadmap
=======
- [x] Figure out good enough encoding for JSON Schema
- [x] Encode inline definitions (i.e. no `$ref`s)
- [ ] Figure out type-level way to encode `$ref` (probably split `WriteDefinition` into two typeclasses)
- [ ] Introduce way to write a collection of schemas to string
- [ ] Start looking at encoding paths (e.g. `POST /user`)
