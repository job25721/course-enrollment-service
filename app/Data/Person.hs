{-# LANGUAGE DeriveGeneric #-}

module Data.Person (Student (..)) where

import Data.Aeson hiding (json)
import GHC.Generics

data Student = Student
  { studentId :: Int,
    firstName :: [Char],
    lastName :: [Char],
    year :: Int,
    academicYear :: Int,
    faculty :: [Char],
    major :: [Char],
    advisor :: [Char]
  }
  deriving (Generic, Show, Ord, Eq)

instance ToJSON Student

instance FromJSON Student