module Data.JSON.Definition
  ( Definition(..)
  , Reference(..)
  , Description(..)

  , class JsonSchema
  , definition
  , description
  , schemaPath

  , class RecordDefinition
  , recordJsonSchema

  , class IsType
  , typeOf
  , TypeOf

  , class RowToProperty
  , toPropertyArray

  , showJson
  , showYaml
  ) where

import Prelude

import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (Foreign)
import Foreign.Object as FO
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Row as Row
import Simple.JSON (write, writeJSON)
import Type.Prelude (RLProxy(..))
import Type.Proxy (Proxy(..))

import Data.JSON.Schema (Schema(..), Property(..), Object(..), StringFormat(..))

newtype Definition a = Definition Schema

derive instance newtypeDefinition :: Newtype (Definition a) _

derive newtype instance showDefinition :: Show (Definition a)
derive newtype instance eqDefinition :: Eq (Definition a)

newtype Reference a = Ref String

derive instance newtypeReference :: Newtype (Reference a) _

newtype Description a = Description String

derive instance newtypeDescription :: Newtype (Description a) _

class JsonSchema a where
  definition :: Definition a
  description :: Maybe (Description a)
  schemaPath :: Reference a

class RecordDefinition a where
  recordJsonSchema :: Definition a

class IsType a where
  typeOf :: Proxy a -> TypeOf

data TypeOf = BuiltinType | RefType | ArrayType TypeOf

derive instance eqTypeOf :: Eq TypeOf

instance jsonSchemaString :: JsonSchema String where
  definition = Definition (String None)
  description = Nothing
  schemaPath = Ref "<INVALID>"

instance jsonSchemaInt :: JsonSchema Int where
  definition = Definition Int
  description = Nothing
  schemaPath = Ref "<INVALID>"

instance jsonSchemaNumber :: JsonSchema Number where
  definition = Definition Number
  description = Nothing
  schemaPath = Ref "<INVALID>"

instance jsonSchemaBoolean :: JsonSchema Boolean where
  definition = Definition Boolean
  description = Nothing
  schemaPath = Ref "<INVALID>"

instance jsonSchemaArray :: JsonSchema a => JsonSchema (Array a) where
  definition = Definition $ Array $ unwrap (definition :: Definition a)
  description = Nothing
  schemaPath = Ref $ unwrap (schemaPath :: Reference a)

instance basicTypeString :: IsType String where
  typeOf _ = BuiltinType

else instance basicTypeInt :: IsType Int where
  typeOf _ = BuiltinType

else instance basicTypeArray :: IsType a => IsType (Array a) where
  typeOf _ = ArrayType $ typeOf (Proxy :: Proxy a)

else instance basicTypeMaybe :: IsType a => IsType (Maybe a) where
  typeOf _ = typeOf (Proxy :: Proxy a)

else instance basicTypeOther :: IsType a where
  typeOf _ = RefType

-- | Write a `Definition a` to a JSON string
showJson :: ∀ a. Definition a -> String
showJson = writeJSON <<< writeDefinition

-- | Write a `Definition a` to a YAML string
showYaml :: ∀ a. Definition a -> String
showYaml = writeYaml <<< writeDefinition

foreign import writeYaml :: Foreign -> String

writeDefinition :: ∀ a. Definition a -> Foreign
writeDefinition (Definition s) = writeSchema s

writeSchema :: Schema -> Foreign
writeSchema Int = write { "type": "integer" }
writeSchema Number = write { "type": "number" }
writeSchema Boolean = write { "type": "boolean" }
writeSchema (Array s) = write { "type": "array", "items": writeSchema s }
writeSchema (Reference r) = write { "$ref": r }
writeSchema (String fmt) = case format fmt of
  Just f -> write { "type": "string", format: f }
  Nothing -> write { "type": "string" }
  where
    format None = Nothing
    format Date = Just "date"
    format DateTime = Just "date-time"
    format Password = Just "password"
    format Byte = Just "byte"
    format Binary = Just "binary"
writeSchema (Object (Properties ps)) =
  if any (\(Property r _ _) -> r) ps
    then write { "type": "object", required: ps >>= required, properties: props }
    else write { "type": "object", properties: props }
  where
    required (Property true n _) = [n]
    required _ = []
    props = FO.fromFoldable $ writePropTuple <$> ps

writePropTuple :: Property -> Tuple String Foreign
writePropTuple (Property _ n s) = Tuple n $ writeSchema s

instance recordUnamedSchema ::
  ( RowToList fields fieldList
  , RowToProperty fieldList
  ) => RecordDefinition (Record fields) where
  recordJsonSchema = Definition $ Object $ Properties $ toPropertyArray fieldListP
    where
      fieldListP = RLProxy :: RLProxy fieldList

class RowToProperty (xs :: RowList Type) where
  toPropertyArray :: RLProxy xs -> Array Property

instance writeDefinitionFieldsMaybeCons ::
  ( IsSymbol name
  , IsType head
  , JsonSchema head
  , RowToProperty tail
  , Row.Cons name (Maybe head) trash row
  ) => RowToProperty (Cons name (Maybe head) tail) where
  toPropertyArray _ = [Property false (reflectSymbol nameP) schema] <> toPropertyArray rest
    where
      schema = case typeOf (Proxy :: Proxy head) of
        BuiltinType -> unwrap (definition :: Definition head)
        RefType -> Reference $ unwrap (schemaPath :: Reference head)
        ArrayType tp ->
          if tp == BuiltinType
            then unwrap (definition :: Definition head)
            else Array $ Reference $ unwrap (schemaPath :: Reference head)
      nameP = SProxy :: SProxy name
      rest = RLProxy :: RLProxy tail

else instance rowToPropertyCons ::
  ( IsSymbol name
  , IsType head
  , JsonSchema head
  , RowToProperty tail
  , Row.Cons name head trash row
  ) => RowToProperty (Cons name head tail) where
  toPropertyArray _ = [Property true (reflectSymbol nameP) schema] <> toPropertyArray rest
    where
      schema = case typeOf (Proxy :: Proxy head) of
        BuiltinType -> unwrap (definition :: Definition head)
        RefType -> Reference $ unwrap (schemaPath :: Reference head)
        ArrayType tp ->
          if tp == BuiltinType
            then unwrap (definition :: Definition head)
            else Array $ Reference $ unwrap (schemaPath :: Reference head)
      nameP = SProxy :: SProxy name
      rest = RLProxy :: RLProxy tail

instance rowToPropertyNil :: RowToProperty Nil where
  toPropertyArray _ = []
