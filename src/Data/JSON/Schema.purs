module Data.JSON.Schema
  ( Schema(..)
  , Object(..)
  , Property(..)
  , StringFormat(..)
  , Required
  , Name
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foldable (elem, all)
import Data.Array (length)

-- | The different schema definitions, strings, arrays, objects and so on
data Schema
  = Object Object
  | String StringFormat
  | Number
  | Boolean
  | Int
  | Array Schema
  | Reference String

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

instance eqObject :: Eq Object where
  eq (Properties xs1) (Properties xs2) =
    length xs1 == length xs2 &&
    all (flip elem $ xs1) xs2

data Property = Property Required Name Schema

derive instance genericProperty :: Generic Property _

instance showProperty :: Show Property where
  show = genericShow

derive instance eqProperty :: Eq Property

type Required = Boolean

type Name = String
