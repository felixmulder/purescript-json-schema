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
