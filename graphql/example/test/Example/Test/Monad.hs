{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeInType #-}

module Example.Test.Monad where

import Control.Monad.IO.Class (MonadIO)
import Data.GraphQL (MonadQuery, mockedQuerySettings, runQueryT)

import Example (App(..))
import Example.GraphQL.API (API)
import Example.Test.Mock (MockData)

newtype TestApp a = TestApp (App a)
  deriving (Functor,Applicative,Monad,MonadIO,MonadQuery API)

runMockApp :: TestApp a -> MockData -> IO a
runMockApp (TestApp (App queryT)) mock = runQueryT (mockedQuerySettings mock) queryT
