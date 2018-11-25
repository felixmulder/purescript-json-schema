module Test.Main where

import Prelude (Unit, discard, ($))

import Effect (Effect)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Data.Maybe (Maybe)
import Data.JSON.Schema (class WriteDefinition, Definition(..), Object(..), Property(..), Schema(..), StringFormat(..), definition)

-- | This test aims to implement support for the following definition:
-- |
-- | ```
-- | User:
-- |   type: object
-- |   required: [name, parents]
-- |   properties:
-- |     name:
-- |       type: string
-- |     age:
-- |       type: number
-- |     parents:
-- |       type: array
-- |       items:
-- |         '$ref': '#/definitions/Parents'
-- | ```
main :: Effect Unit
main = run [consoleReporter] do
  describe "purescript-json-schema" do
    it "create correct fields for { name :: String, age :: Age }" do
      (definition :: Definition { name :: String, age :: Age }) `shouldEqual` nameAgeDef

    pending "create correct fields for { name :: String, age :: Maybe Age }" -- do
      --(definition :: Definition { name :: String, age :: Maybe Age }) `shouldEqual` nameMaybeAgeDef

    pending "create correct fields for { name :: String, age :: Maybe Age, parents :: Array Parent }"

type User =
  { name :: String
  , age :: Age
  }

newtype Age = Age Int

instance writeDefinitionAge :: WriteDefinition Age where
  definition = Definition Int

nameAgeDef :: Definition { name :: String, age :: Age }
nameAgeDef = Definition $ Object $ Properties [ ageDef, nameDef ]
  where
    nameDef = Property true "name" (String None)
    ageDef = Property true "age" Int

nameMaybeAgeDef :: Definition { name :: String, age :: Maybe Age }
nameMaybeAgeDef = Definition $ Object $ Properties [ ageDef, nameDef ]
  where
    nameDef = Property true "name" (String None)
    ageDef = Property false "age" Int
