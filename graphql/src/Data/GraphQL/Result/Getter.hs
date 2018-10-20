{-|
Module      :  Data.GraphQL.Result.Getter
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for querying GraphQL results using quasiquoters.

'Schema' defines the format a successful result is guaranteed to come back as. We can then use this
schema to generate expressions we know are safe; e.g. if we know that the @a@ key in a JSON object
is a 'Bool', we can use the 'getBool' function:

@
getBool :: Value -> Bool
getBool = \case
  Bool b -> b
  _ -> error "Invalid boolean"
@

Even though this function is partial (i.e. calls 'error'), we know it won't error because the @a@
key is typed (by GraphQL) to be a 'Bool'.

To get even more type-safety, we can generate these expressions at compile-time and ensure that
'getBool' is only called for @a@ and not for another key that might not be 'Bool'. Using
'getterFor', we can write expressions like the following:

@
-- Foo.hs
schema :: Schema
schema = SchemaObject
  [ ("foo", SchemaObject
      [ ("a", SchemaInt)
      , ("nodes", SchemaList $ SchemaObject
          [ ("b", SchemaMaybe SchemaBool)
          ]
        )
      , ("c", SchemaText)
      ]
    )
  ]

get :: QuasiQuoter
get = getterFor schema
@

@
-- Bar.hs
doFoo = do
  result <- runQuery ...

  [get| result.foo.a |]          :: Int
  [get| result.foo.nodes |]      :: [Object]
  [get| result.foo.nodes[] |]    :: [Object]
  [get| result.foo.nodes[].b |]  :: [Maybe Bool]
  [get| result.foo.nodes[].b! |] :: [Bool] -- would error at runtime if any "b" values are null
  [get| result.foo.c |]          :: Text

  let nodes = [get| result.foo.nodes[] > node |]
  flip map nodes $ \\node -> case [get| \@node.b |] of
    Just b -> b
    Nothing -> [get| \@node.a |] == 0
@

These "getter" expressions follow the given rules:

* @x@ returns the value of @x@ with the given type:

    * @SchemaBool@ returns a 'Bool'
    * @SchemaInt@ returns an 'Int'
    * @SchemaDouble@ returns a 'Double'
    * @SchemaText@ returns a 'Text.Text'
    * @SchemaScalar@ returns a 'Text.Text' (TODO: provide a function to parse this)
    * @SchemaEnum (Proxy :: Proxy a)@ returns a value of type 'a'
    * @SchemaMaybe schema@ returns a 'Maybe' value wrapping the value returned by the inner schema
    * @SchemaList schema@ returns a list of values, whose type is determined by the inner schema
    * @SchemaObject fields@ returns an 'Data.Aeson.Object'

* @x.y@ is only valid if @x@ is a @SchemaObject@ or a @SchemaMaybe SchemaObject@. Returns the value
  of the key @y@ in the (potentially wrapped) 'Object'.

* @x!@ is only valid if @x@ is a @SchemaMaybe@. Unwraps the value of @x@ from a 'Just' value and
  errors (at runtime!) if @x@ is 'Nothing'.

* @x[]@ is only valid if @x@ is a @SchemaList@. Applies the remaining rules as an 'fmap' over the
  values in the list.

    * @x[]@ without anything after is equivalent to @x@
    * @x[].y@ gets the key @y@ in all the Objects in @x@
    * @x[]!@ unwraps all 'Just' values in @x@ (and errors if any 'Nothing' values exist in @x@)

* @> name@ is only valid at the end of a getter for a value containing an 'Object'. Stores the
  schema of the contained 'Object' for subsequent queries. After storing the schema for @name@,
  the schema can be used by prefixing the variable with @\@@: @[get| \@name.bar.etc |]@.

    * Can store the schema of a plain @Object@, a @Maybe Object@, or a @[Object]@.

        @
        let obj = [get| result.foo > obj |]
            a = [get| \@obj.a |]
            b = [get| \@obj.b |]

        let mayb = [get| result.bar > inner |]
            a = case mayb of
              Just inner -> [get| \@inner.a |]
              Nothing -> [get| result.a |]

        let list = [get| result.baz[] > elem |]
            as = map (\\elem -> [get| \@elem.a |]) list
            bs = map (\\elem -> [get| \@elem.b |]) list
        @

    * @name@ needs to match the name of the value being queried. The following won't work:

        @
        let nodes = [get| result.foo.nodes[] > node |]
            bs = flip map nodes $ \\n -> fromMaybe True [get| @n.b |]
                                                           -- ^ BAD: 'n' /= 'node'
        @

        This example will error, saying that a schema for @n@ is not stored.

    * The schema needs to be stored before being used. The following won't work:

        @
        do
          result <- runQuery ...
          let nodes = [get| result.foo.nodes[] > node |]
          return $ map fromNode nodes
        where
          fromNode node = [get| \@node.b |]
        @

        Since @fromNode@ is in a where clause, it's compiled before the @get@ quote that stores
        the schema of @node@, so this example will error.

    * The stored schema is global to the module (but can only be used after being stored) and is
      namespaced to the type being queried.

        @
        [Foo.get| result.nodes[] > node |] -- stored as Foo.node
        [Bar.get| result.nodes[] > node |] -- stored as Bar.node
        [Foo.get| \@node.foo |]             -- uses Foo.node
        [Bar.get| result.other > node |]   -- errors that Bar.node is already stored
        @
-}

module Data.GraphQL.Result.Getter
  ( getterFor
  -- * Re-exports
  , QuasiQuoter
  ) where

import Language.Haskell.TH (Name)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.GraphQL.Result.Getter.Internal (generateGetter)
import Data.GraphQL.Result.Schema (Schema)

-- | Return a QuasiQuoter that can parse the given schema.
getterFor :: Name -> Schema -> QuasiQuoter
getterFor resultCon fullSchema = QuasiQuoter
  { quoteExp = generateGetter resultCon fullSchema
  , quotePat = invalid
  , quoteType = invalid
  , quoteDec = invalid
  }
  where
    invalid _ = error "A getter quasiquote can only used as an expression"
