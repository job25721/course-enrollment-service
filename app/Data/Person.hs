{-# LANGUAGE DeriveGeneric #-}

module Data.Person (Student (..), Teacher (..), Users (..), User (..), StdAuth (..)) where

import Data.Aeson hiding (json)
import GHC.Generics

data Users = Users
  { studnets :: [Student]
  }
  deriving (Generic, Show)

instance ToJSON Users

instance FromJSON Users

data User = User
  { firstName :: [Char],
    lastName :: [Char],
    userType :: [Char]
  }
  deriving (Generic, Show, Ord, Eq)

instance ToJSON User

instance FromJSON User

data Student = Student
  { studentId :: Int,
    year :: Int,
    academicYear :: Int,
    faculty :: [Char],
    major :: [Char],
    advisor :: [Char],
    studentInfo :: User
  }
  deriving (Generic, Show, Ord, Eq)

instance ToJSON Student

instance FromJSON Student

data Teacher = Teacher
  { teacherId :: Int,
    teacherEmail :: [Char],
    teacherInfo :: User
  }
  deriving (Generic, Show, Ord, Eq)

instance ToJSON Teacher

instance FromJSON Teacher

data StdAuth = StdAuth
  { stdId :: Int
  }
  deriving (Generic, Show)

instance ToJSON StdAuth

instance FromJSON StdAuth
