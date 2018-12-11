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
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parse :: Monad m => Parser a -> String -> m a
parse parser s = either (fail . parseErrorPretty) return $ runParser parser s s

type GetterOps = [GetterOperation]

data GetterOperation
  = GetterKey String
  | GetterKeyList [GetterOps]
  | GetterKeyTuple [GetterOps]
  | GetterBang
  | GetterMapList
  | GetterMapMaybe
  deriving (Show)

getterOp :: Parser GetterOperation
getterOp = choice
  [ string "!" $> GetterBang
  , string "[]" $> GetterMapList
  , string "?" $> GetterMapMaybe
  , optional (string ".") *> choice
      [ GetterKey <$> identifier lowerChar
      , fmap GetterKeyList $ between (string "[") (string "]") $ some getterOp `sepBy1` string ","
      , fmap GetterKeyTuple $ between (string "(") (string ")") $ some getterOp `sepBy1` string ","
      ]
  ]

identifier :: Parser Char -> Parser String
identifier start = (:) <$> start <*> many (alphaNumChar <|> char '\'')

lexeme :: String -> Parser ()
lexeme s = space >> string s >> space

{- GetterExp -}

data GetterExp = GetterExp
  { start     :: Maybe String
  , getterOps :: GetterOps
  } deriving (Show)

getterExp :: Parser GetterExp
getterExp = do
  space
  start <- optional $ identifier lowerChar
  getterOps <- many getterOp
  space
  void eof
  return GetterExp{..}

{- GetterDecs -}

data GetterDecs = GetterDecs
  { startSchema :: String
  , getterOps   :: GetterOps
  , endSchema   :: String
  } deriving (Show)

getterDecs :: Parser GetterDecs
getterDecs = do
  space
  startSchema <- identifier upperChar
  lexeme ">"
  getterOps <- many getterOp
  lexeme ">"
  endSchema <- identifier upperChar
  space
  void eof
  return GetterDecs{..}
