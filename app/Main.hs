{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Route

import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.IORef
import Data.Semigroup ((<>))
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

enroll :: Int -> [Course] -> [Course]
enroll cid = map (\course -> if courseId course == cid then Course {courseId = courseId course, name = name course, credit = credit course, seat = (-) (seat course) 1, enrolled = (+) (enrolled course) 1} else course)

findCourse :: Int -> [Course] -> Maybe Course
findCourse _ [] = Nothing
findCourse cid (x : xs)
  | courseId x == cid = Just x
  | otherwise = findCourse cid xs

newtype ServerState = ServerState {courses :: IORef [Course]}

type Api a = SpockM () () ServerState a

type ApiAction a = SpockAction () () () a

app :: Api ()
app = do
  get "/api/courses" $ do
    courses' <- getState >>= (liftIO . readIORef . courses)
    json $ courses'
  get "/api/course" $ do
    courses' <- getState >>= (liftIO . readIORef . courses)
    cid <- param' "cid"
    json $ findCourse cid courses'
  get "/api/add" $ do
    courseId <- param' "cid"
    name <- param' "cname"
    credit <- param' "credit"
    seat <- param' "seat"
    enrolled <- param' "enrolled"
    coursesRef <- courses <$> getState
    liftIO $
      atomicModifyIORef' coursesRef $ \courses ->
        (courses <> [Course {courseId = courseId, name = name, credit = credit, seat = seat, enrolled = enrolled}], ())
    text $ "added" <> pack (show Course {courseId = courseId, name = name, credit = credit, seat = seat, enrolled = enrolled})
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
  st <- ServerState <$> newIORef [Course {courseId = 261497, name = "Functional Programming", credit = 3, seat = 10, enrolled = 3}, Course {courseId = 261208, name = "Basic Computer Engr", credit = 3, seat = 100, enrolled = 29}]
  spockCfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 3000 (spock spockCfg app)