{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Course (Course (..))
import Data.IORef
import Data.Text (pack)
import Functions (allCourses, enroll, findCourse)
import Web.Spock
import Web.Spock.Config

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
  post "/api/enroll" $ do
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