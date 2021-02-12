{-# LANGUAGE DeriveGeneric #-}

module Data.Course (Course (..), Section (..)) where

import Data.Aeson hiding (json)
import Data.Person (Student (..))
import GHC.Generics

data Course = Course
  { courseId :: Int,
    name :: [Char],
    credit :: Int,
    sections :: [Section]
  }
  deriving (Generic, Show)

instance ToJSON Course

instance FromJSON Course

data Section = Section
  { sectionId :: Int,
    seat :: Int,
    enrolledPerson :: [Student]
  }
  deriving (Generic, Show)

instance ToJSON Section

instance FromJSON Section