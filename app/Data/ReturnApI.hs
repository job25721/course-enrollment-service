{-# LANGUAGE DeriveGeneric #-}

module Data.ReturnApi (ApiResponse (..)) where

import Data.Aeson hiding (json)
import Data.Course (Course)
import Data.Person (Student)
import GHC.Generics

data ApiResponse = ApiResponse
  { message :: [Char],
    dataResponse :: Maybe Course
  }
  deriving (Generic, Show)

instance ToJSON ApiResponse

instance FromJSON ApiResponse
