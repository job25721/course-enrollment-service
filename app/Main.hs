{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Route

import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.IORef
import Data.Text (pack)
import GHC.Generics
import Web.Spock
import Web.Spock.Config

-- data Person = Person
--   { name :: [Char],
--     age :: Int
--   }
--   deriving (Generic, Show)

-- instance ToJSON Person

-- instance FromJSON Person
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

data ApiResponse = ApiResponse
  { message :: [Char],
    res :: [Course]
  }
  deriving (Generic, Show)

instance ToJSON ApiResponse

instance FromJSON ApiResponse

allCourses :: [Course]
allCourses =
  [ Course {courseId = 261497, name = "Functional Programming", credit = 3, seat = 10, enrolled = 3},
    Course {courseId = 261208, name = "Basic Computer Engr", credit = 3, seat = 100, enrolled = 29}
  ]

enroll :: Int -> [Course] -> [Course]
enroll cid = map (\course -> if courseId course == cid then Course {courseId = courseId course, name = name course, credit = credit course, seat = (-) (seat course) 1, enrolled = (+) (enrolled course) 1} else course)

findCourse :: Int -> [Course] -> Maybe Course
findCourse _ [] = Nothing
findCourse cid (x : xs)
  | courseId x == cid = Just x
  | otherwise = findCourse cid xs

newtype ServerState = ServerState {courses :: IORef [Course]}

type Api a = SpockM () () ServerState a

type ApiAction a = SpockAction () () ServerState a

app :: Api ()
app = do
  get "/api/courses" $ do
    courses' <- getState >>= (liftIO . readIORef . courses)
    json courses'
  get "/api/course" $ do
    courses' <- getState >>= (liftIO . readIORef . courses)
    cid <- param' "cid"
    json $ findCourse cid courses'
  post "/api/add" $ do
    newCourse <- jsonBody' :: ApiAction Course
    coursesRef <- courses <$> getState
    liftIO $
      atomicModifyIORef' coursesRef $ \courses ->
        (courses ++ [newCourse], ())
    text $ "added" <> pack (show newCourse)
  get "/api/enroll" $ do
    cid <- param' "cid"
    coursesRef <- courses <$> getState
    liftIO $
      atomicModifyIORef' coursesRef $ \courses ->
        (enroll cid courses, ())
    courses' <- getState >>= (liftIO . readIORef . courses)
    json $ "enrolled " <> pack (show (findCourse cid courses'))

main :: IO ()
main = do
  st <- ServerState <$> newIORef allCourses
  spockCfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 3000 (spock spockCfg app)