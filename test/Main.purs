module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.JSON.Schema (Object(..), Property(..), Schema(..), StringFormat(..))
import Data.JSON.Definition (class JsonSchema, Definition(..), Reference, recordJsonSchema, definition, schemaPath)

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
    it "generate for newtyped String" do
      definition `shouldEqual` userNameDef

    it "generate for { username :: UserName }" do
      recordJsonSchema `shouldEqual` userWithUserName

    it "generate for { string :: String }" do
      recordJsonSchema `shouldEqual` recordWithString

    it "generate for { username :: Maybe UserName, address :: String }" do
      recordJsonSchema `shouldEqual` withMaybeUserNameDef

    it "generate for { arr :: Array String }" do
      recordJsonSchema `shouldEqual` recordWithArr

    it "generate for { parents :: Array Parent }" do
      recordJsonSchema `shouldEqual` parents

    it "generate for { ages :: Maybe (Array String) }" do
      recordJsonSchema `shouldEqual` recordWithMaybeArr

    it "generate for { name :: UserName, age :: Maybe Int, parents :: Array Parent }" do
      recordJsonSchema `shouldEqual` userDefinition

    where
      userDefinition :: Definition { name :: UserName, age :: Maybe Int, parents :: Array Parent }
      userDefinition =
        Definition $ Object $ Properties
          [ Property true "name" $ Reference "#/definitions/UserName"
          , Property false "age" Int
          , Property true "parents" $ Array $ Reference "#/definitions/Parent"
          ]

      parents :: Definition { parents :: Array Parent }
      parents =
        Definition $ Object $ Properties
          [ Property true "parents" $ Array $ Reference "#/definitions/Parent" ]

      recordWithString :: Definition { string :: String }
      recordWithString =
        Definition $ Object $ Properties [ Property true "string" $ String None ]

      userWithUserName :: Definition { username :: UserName }
      userWithUserName =
        Definition $ Object $
        Properties [ Property true "username" $ Reference "#/definitions/UserName" ]

      recordWithArr :: Definition { arr :: Array String }
      recordWithArr = Definition $ Object $ Properties [ Property true "arr" $ Array (String None) ]

      recordWithMaybeArr :: Definition { arr :: Maybe (Array String) }
      recordWithMaybeArr = Definition $ Object $ Properties [ Property false "arr" $ Array (String None) ]

      userNameDef :: Definition UserName
      userNameDef = Definition (String None)

      withMaybeUserNameDef :: Definition { username :: Maybe UserName, address :: String }
      withMaybeUserNameDef =
        Definition $ Object $
        Properties [ Property true "address" $ String None
                   , Property false "username" $ Reference "#/definitions/UserName"
                   ]

newtype UserName = UserName String

instance jsonSchemaUserName :: JsonSchema UserName where
  schemaPath = wrap "#/definitions/UserName"
  definition = wrap $ unwrap $ definition :: Definition String

newtype Parent = Parent UserName

instance jsonSchemaParent :: JsonSchema Parent where
  schemaPath = wrap "#/definitions/Parent"
  definition = wrap $ Reference $ unwrap (schemaPath :: Reference UserName)
