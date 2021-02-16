{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Course (Course (..))
import Data.IORef
import Data.Text (pack)
import Data.Text.Lazy.IO as I
import Functions (alreadyEnroll, dropCourse, enroll, findCourse)
import Store
import Web.Spock
import Web.Spock.Config

newtype ServerState = ServerState {courses :: IORef [Course]}

type Api a = SpockM () () ServerState a

type ApiAction a = SpockAction () () ServerState a

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x

resetFile :: IO ()
resetFile = I.writeFile "courses.json" (encodeToLazyText allCourses)

decodeCoursesArray :: B.ByteString -> [Course]
decodeCoursesArray courses' = fromMaybe (decode courses' :: Maybe [Course])

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
    courses' <- getState >>= (liftIO . readIORef . courses)
    liftIO $ I.writeFile "courses.json" $ encodeToLazyText courses'
    text $ "added" <> pack (show newCourse)
  post "/api/enroll" $ do
    cid <- param' "cid"
    secId <- param' "secId"
    studentId <- param' "studentId"
    coursesRef <- courses <$> getState
    courses' <- getState >>= (liftIO . readIORef . courses)
    liftIO $
      atomicModifyIORef' coursesRef $ \courses ->
        (enroll cid secId studentId courses, ())
    courses'' <- getState >>= (liftIO . readIORef . courses)
    liftIO $ I.writeFile "courses.json" $ encodeToLazyText courses''
    json $
      if alreadyEnroll studentId cid secId courses'
        then "already enrolled"
        else "enrolled " <> pack (show (findCourse cid courses''))
  post "/api/drop" $ do
    cid <- param' "cid"
    secId <- param' "secId"
    studentId <- param' "studentId"
    coursesRef <- courses <$> getState
    courses' <- getState >>= (liftIO . readIORef . courses)
    liftIO $
      atomicModifyIORef' coursesRef $ \courses ->
        (dropCourse cid secId studentId courses, ())
    courses'' <- getState >>= (liftIO . readIORef . courses)
    liftIO $ I.writeFile "courses.json" $ encodeToLazyText courses''
    json $
      if not $ alreadyEnroll studentId cid secId courses'
        then "you haven't enroll this course"
        else "course dropped " <> pack (show (findCourse cid courses'))

main :: IO ()
main = do
  courses' <- liftIO $ B.readFile "courses.json"
  st <- ServerState <$> newIORef (decodeCoursesArray courses')
  spockCfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 3000 (spock spockCfg app)