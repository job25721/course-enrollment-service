{-# LANGUAGE DeriveGeneric #-}

module Data.Database (Db (..)) where

import Data.Aeson hiding (json)
import Data.Course (Course)
import Data.Person (Users (..))
import GHC.Generics

data Db = Db
  { courses :: [Course],
    users :: Users
  }
  deriving (Generic, Show)

instance ToJSON Db

instance FromJSON Db