{-|
Module      :  Data.GraphQL.TH.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for parsing input text in QuasiQuoters.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.GraphQL.TH.Parse where

import Control.Monad (void)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: Monad m => Parser a -> String -> m a
parse parser s = either (fail . parseErrorPretty) return $ runParser parser s s

type GetterOps = [GetterOperation]

data GetterOperation
  = GetterKey String
  | GetterList [GetterOps]
  | GetterTuple [GetterOps]
  | GetterBang
  | GetterMapList
  | GetterMapMaybe
  deriving (Show)

getterOp :: Parser GetterOperation
getterOp = choice
  [ lexeme "!" $> GetterBang
  , lexeme "[]" $> GetterMapList
  , lexeme "?" $> GetterMapMaybe
  , optional (lexeme ".") *> choice
      [ GetterKey <$> identifier lowerChar
      , fmap GetterList $ between (lexeme "[") (lexeme "]") $ some getterOp `sepBy1` lexeme ","
      , fmap GetterTuple $ between (lexeme "(") (lexeme ")") $ some getterOp `sepBy1` lexeme ","
      ]
  ]

identifier :: Parser Char -> Parser String
identifier start = (:) <$> start <*> many (alphaNumChar <|> char '\'')

lexeme :: String -> Parser ()
lexeme = void . L.lexeme (L.space space1 empty empty) . string

-- | Parses `identifier`, but if parentheses are provided, parses a namespaced identifier.
namespacedIdentifier :: Parser Char -> Parser String
namespacedIdentifier start = choice [lexeme "(" *> namespaced <* lexeme ")", ident]
  where
    ident = identifier start
    namespaced = intercalate "." <$> manyAndEnd (identifier upperChar <* lexeme ".") ident
    manyAndEnd p end = choice
      [ try $ p >>= \x -> (x:) <$> manyAndEnd p end
      , (:[]) <$> end
      ]

{- GetterExp -}

data GetterExp = GetterExp
  { start     :: Maybe String
  , getterOps :: GetterOps
  } deriving (Show)

getterExp :: Parser GetterExp
getterExp = do
  space
  start <- optional $ namespacedIdentifier lowerChar
  getterOps <- many getterOp
  space
  void eof
  return GetterExp{..}

{- UnwrapSchema -}

data UnwrapSchema = UnwrapSchema
  { startSchema :: String
  , getterOps   :: GetterOps
  } deriving (Show)

unwrapSchema :: Parser UnwrapSchema
unwrapSchema = do
  space
  startSchema <- namespacedIdentifier upperChar
  getterOps <- many getterOp
  space
  void eof
  return UnwrapSchema{..}
