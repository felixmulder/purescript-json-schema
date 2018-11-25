module Data.JSON.Schema
  ( Definition(..)
  , Schema(..)
  , Object(..)
  , Property(..)
  , StringFormat(..)
  , Reference
  , Required
  , Name

  , class WriteDefinition
  , definition

  , jsonDefinition

  , class WriteDefinitionFields
  , toArray
  ) where

import Prelude

import Foreign (Foreign)
import Foreign.Object as FO
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Simple.JSON (write, writeJSON)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Prelude (RLProxy(..))


-- | A JSON Schema definition according to OpenAPI 3.0.3
newtype Definition a = Definition Schema

instance showDefinition :: Show (Definition a) where
  show (Definition s) = "(Definition " <> show s <> ")"

derive newtype instance eqDefinition :: Eq (Definition a)

unDef :: forall a. Definition a -> Schema
unDef (Definition s) = s

-- | The different schema definitions, strings, arrays, objects and so on
data Schema
  = Object Object
  | String StringFormat
  | Number
  | Boolean
  | Int
  | Array Schema
  | Reference Reference

instance showSchema :: Show Schema where
  show (Object obj) = "(Object " <> show obj <> ")"
  show (String fmt) = "(String " <> show fmt <> ")"
  show Number = "Number"
  show Boolean = "Boolean"
  show Int = "Int"
  show (Array s) = "(Array " <> show s <> ")"
  show (Reference r) = "(Reference " <> r <> ")"

derive instance eqSchema :: Eq Schema

-- | The different string format
data StringFormat
  = None
  | Date
  | DateTime
  | Password
  | Byte
  | Binary

derive instance genericStringFormat :: Generic StringFormat _

instance showStringFormat :: Show StringFormat where
  show = genericShow
derive instance eqStringFormat :: Eq StringFormat

data Object
  = Properties (Array Property)

derive instance genericObject :: Generic Object _

instance showObject :: Show Object where
  show = genericShow
derive instance eqObject :: Eq Object

data Property = Property Required Name Schema

derive instance genericProperty :: Generic Property _

instance showProperty :: Show Property where
  show = genericShow
derive instance eqProperty :: Eq Property

type Reference = String

type Required = Boolean

type Name = String

class WriteDefinition a where
  definition :: Definition a

-- | Write a `Definition a` to a JSON string
jsonDefinition :: forall a. Definition a -> String
jsonDefinition = writeJSON <<< writeDefinition

writeDefinition :: forall a. Definition a -> Foreign
writeDefinition (Definition s) = writeSchema s

writeSchema :: Schema -> Foreign
writeSchema Int = write { "type": "integer" }
writeSchema Number = write { "type": "number" }
writeSchema Boolean = write { "type": "boolean" }
writeSchema (Array s) = write { "type": "array", "items": writeSchema s }
writeSchema (Reference r) = write { "$ref": r }
writeSchema (String f) = write { "type": "string", format: format f }
  where
    format None = Nothing
    format Date = Just "date"
    format DateTime = Just "date-time"
    format Password = Just "password"
    format Byte = Just "byte"
    format Binary = Just "binary"
writeSchema (Object (Properties ps)) =
  write { "type": "object"
        , required: ps >>= required
        , properties: FO.fromFoldable $ writePropTuple <$> ps
        }
  where
    required (Property true n _) = [n]
    required _ = []

writePropTuple :: Property -> Tuple String Foreign
writePropTuple (Property _ n s) = Tuple n $ writeSchema s

instance stringWriteDefinition :: WriteDefinition String where
  definition = Definition (String None)

instance intWriteDefinition :: WriteDefinition Int where
  definition = Definition Int

instance numberWriteDefinition :: WriteDefinition Number where
  definition = Definition Number

instance recordWriteForeign ::
  ( RowToList fields fieldList
  , WriteDefinitionFields fieldList
  ) => WriteDefinition (Record fields) where
  definition = Definition $ Object $ Properties $ toArray fieldListP
    where
      fieldListP = RLProxy :: RLProxy fieldList

class WriteDefinitionFields (xs :: RowList) where
  toArray :: RLProxy xs -> Array Property

instance writeDefinitionFieldsMaybeCons ::
  ( IsSymbol name
  , WriteDefinition head
  , WriteDefinitionFields tail
  , Row.Cons name (Maybe head) whatever row
  ) => WriteDefinitionFields (Cons name (Maybe head) tail) where
  toArray _ = [Property false (reflectSymbol nameP) $ unDef (definition :: Definition head)] <> toArray rest
    where
      nameP = SProxy :: SProxy name
      rest = RLProxy :: RLProxy tail

else instance writeDefinitionFieldsArrayCons ::
  ( IsSymbol name
  , WriteDefinition head
  , WriteDefinitionFields tail
  , Row.Cons name (Array head) whatever row
  ) => WriteDefinitionFields (Cons name (Array head) tail) where
  toArray _ = [Property true (reflectSymbol nameP) $ Array $ unDef (definition :: Definition head)] <> toArray rest
    where
      nameP = SProxy :: SProxy name
      rest = RLProxy :: RLProxy tail

else instance writeDefinitionFieldsCons ::
  ( IsSymbol name
  , WriteDefinition head
  , WriteDefinitionFields tail
  , Row.Cons name head whatever row
  ) => WriteDefinitionFields (Cons name head tail) where
  toArray _ = [Property true (reflectSymbol nameP) $ unDef (definition :: Definition head)] <> toArray rest
    where
      nameP = SProxy :: SProxy name
      rest = RLProxy :: RLProxy tail


instance writeDefinitionFieldsNil :: WriteDefinitionFields Nil where
  toArray _ = []
