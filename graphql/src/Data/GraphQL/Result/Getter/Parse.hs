{-|
Module      :  Data.GraphQL.Result.Getter.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for parsing getter expressions.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.GraphQL.Result.Getter.Parse where

import Control.Monad (void)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parse :: Monad m => String -> m GetterExpr
parse s = either (fail . parseErrorPretty) return $ runParser getterExpr s s

data GetterExpr = GetterExpr
  { start       :: String
  , useSchema   :: Maybe String
  , getterOps   :: [GetterOperation]
  , storeSchema :: Maybe String
  } deriving (Show)

data GetterOperation
  = GetterKey String
  | GetterKeyList [String]
  | GetterKeyTuple [String]
  | GetterBang
  | GetterList
  deriving (Show)

identifier :: Parser String
identifier = (:) <$> lowerChar <*> many (alphaNumChar <|> char '\'')

getterExpr :: Parser GetterExpr
getterExpr = do
  space
  (start, useSchema) <- getStart
  getterOps <- many getterOp
  space
  storeSchema <- option Nothing $ string ">" *> space *> fmap Just identifier
  space
  void eof
  return GetterExpr{..}

-- | Gets the starting identifier and possibly the stored schema to start with.
--
-- One of:
--   * `var`      -> ("var", Nothing)
--   * `@var`     -> ("var", Just "var")
--   * `@tag var` -> ("var", Just "tag")
getStart :: Parser (String, Maybe String)
getStart = (string "@" *> getStartWithSchema) <|> fmap (, Nothing) identifier
  where
    getStartWithSchema = do
      ident <- identifier
      fmap (, Just ident) $ option ident $ space *> identifier

getterOp :: Parser GetterOperation
getterOp = choice
  [ string "." *> choice
      [ fmap GetterKey identifier
      , fmap GetterKeyList $ between (string "[") (string "]") $ identifier `sepBy1` string ","
      , fmap GetterKeyTuple $ between (string "(") (string ")") $ identifier `sepBy1` string ","
      ]
  , string "!" $> GetterBang
  , string "[]" $> GetterList
  ]
