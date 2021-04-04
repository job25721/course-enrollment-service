{-# LANGUAGE DeriveGeneric #-}

module Data.Course (Course (..), Section (..)) where

import Data.Aeson hiding (json)
import Data.Person (Student (..), Teacher (..))
import GHC.Generics

data Course = Course
  { courseId :: Int,
    name :: [Char],
    credit :: Int,
    lecturer :: Teacher,
    sections :: [Section]
  }
  deriving (Generic, Show)

instance ToJSON Course

instance FromJSON Course

data Section = Section
  { sectionId :: Int,
    seat :: Int,
    enrolledPerson :: [Student],
    day :: [Char],
    time :: [Char],
    room :: [Char]
  }
  deriving (Generic, Show)

instance ToJSON Section

instance FromJSON Section