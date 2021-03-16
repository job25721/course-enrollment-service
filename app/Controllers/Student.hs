{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Student (studentLogin, getStdInfo, getMyEnrolledCourse) where

import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.Text (encodeToLazyText)
import Data.Course (Course)
import Data.Database
import Data.IORef
import Data.Person (StdAuth (..), Student (..))
import Data.Text.Lazy.IO as I
import Functions (findMyEnrolledCourse, findStudent, isJust, isNothing)
import GHC.Generics
import Types
import Web.Spock

data ApiResponse = ApiResponse
  { message :: [Char],
    dataResponse :: Maybe Student
  }
  deriving (Generic, Show)

instance ToJSON ApiResponse

instance FromJSON ApiResponse

studentLogin :: Controller
studentLogin = do
  stdAuth <- jsonBody' :: ApiAction StdAuth
  if isJust $ findStudent (stdId stdAuth)
    then json $ ApiResponse {message = "Login Successful", dataResponse = findStudent (stdId stdAuth)}
    else json $ ApiResponse {message = "Login Failed", dataResponse = Nothing}

getStdInfo :: Controller
getStdInfo = do
  sid <- param' "sid"
  json $ findStudent sid

getMyEnrolledCourse :: Controller
getMyEnrolledCourse = do
  db <- getState >>= (liftIO . readIORef . database)
  sid <- param' "sid"
  json $ findMyEnrolledCourse sid (courses db)
