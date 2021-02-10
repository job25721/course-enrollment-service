{-# LANGUAGE DeriveGeneric #-}

module Data.Course (Course (..)) where

import Data.Aeson hiding (json)
import GHC.Generics

data Course = Course
  { courseId :: Int,
    name :: [Char],
    credit :: Int,
    seat :: Int,
    enrolled :: Int
  }
  deriving (Generic, Show)

instance ToJSON Course

instance FromJSON Course