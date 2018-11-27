PureScript JSON Schema
======================
A proof of concept project generating JSON schema from PureScript types. Not
ready for primetime yet.

Usage
-----
```purescript
type User =
  { name :: String
  , age :: Maybe Int
  }

recordJsonSchema :: Definition User
-- {
--   "type":"object",
--   "required":["name"],
--   "properties":{"age":{"type":"integer"},"name":{"type":"string"}}
-- }
```

Working with refs
-----------------
In order to serialize a `User` properly to:
```yaml
User:
  type: object
  required: [name, parents]
  properties:
    name:
      type: string
    age:
      type: integer
    parents:
      type: array
      items:
        '$ref': '#/definitions/Parent'
```

The following definitions need to be in place:

```purescript
newtype Parent = Parent String

instance parentSchema :: JsonSchema Parent where
  schemaPath = wrap "#/definitions/Parent" -- this'll be used as the def's path
  definition = Definition $ String None    -- string without formatting

recordJsonSchema :: Definition { name :: String, age :: Maybe Int, parents :: Array Parent }
```

Printing definition
-------------------
You can print to two string formats - JSON and YAML. In order to print to YAML,
you need to install `js-yaml` with `npm` or similar.

Roadmap
=======
- [x] Figure out good enough encoding for JSON Schema
- [x] Encode inline definitions (i.e. no `$ref`s)
- [x] Figure out type-level way to encode `$ref` (probably split `WriteDefinition` into two typeclasses)
- [ ] Introduce way to write a collection of schemas to string
- [ ] Start looking at encoding paths (e.g. `POST /user`)
