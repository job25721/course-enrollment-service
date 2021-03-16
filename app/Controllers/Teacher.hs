{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Teacher (teacherLogin, getTeacherInfo, getTeacherAddedCourses) where

import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.Text (encodeToLazyText)
import Data.Database
import Data.IORef
import Data.Person (Teacher (..), TeacherAuth (..))
import Functions (findMyAddedCourse, findTeacher, isJust)
import GHC.Generics
import Types
import Web.Spock

data ApiResponse = ApiResponse
  { message :: [Char],
    dataResponse :: Maybe Teacher
  }
  deriving (Generic, Show)

instance ToJSON ApiResponse

instance FromJSON ApiResponse

teacherLogin :: Controller
teacherLogin = do
  teacherAuth <- jsonBody' :: ApiAction TeacherAuth
  if isJust $ findTeacher (tEmail teacherAuth)
    then json $ ApiResponse {message = "Login Successful", dataResponse = findTeacher (tEmail teacherAuth)}
    else json $ ApiResponse {message = "Login Failed", dataResponse = Nothing}

getTeacherInfo :: Controller
getTeacherInfo = do
  email <- param' "email"
  json $ findTeacher email

getTeacherAddedCourses :: Controller
getTeacherAddedCourses = do
  email <- param' "email"
  db <- getState >>= (liftIO . readIORef . database)
  json $ findMyAddedCourse email $ courses db