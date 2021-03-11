{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Course (Course (..))
import Data.Database (Db (..))
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import Data.Person (StdAuth (..), Student (..), Users (..))
import Data.ReturnApi (ApiResponse (..))
import Data.Text (pack)
import Data.Text.Lazy.IO as I (writeFile)
import Functions (alreadyEnroll, dropCourse, enroll, findCourse, findStudent, isJust, isNothing)
import Store (allCourses, students)
import Web.Spock
import Web.Spock.Config
  ( PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
  )

newtype ServerState = ServerState {database :: IORef Db}

type Api a = SpockM () () ServerState a

type ApiAction a = SpockAction () () ServerState a

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x

resetFile :: IO ()
resetFile =
  I.writeFile "db.json" $
    encodeToLazyText Db {courses = allCourses, users = Users {studnets = students}}

decodeCoursesArray :: B.ByteString -> [Course]
decodeCoursesArray courses' = fromMaybe (decode courses' :: Maybe [Course])

decodeToJson :: B.ByteString -> Db
decodeToJson db = fromMaybe (decode db :: Maybe Db)

corsHeader :: ActionCtxT b (WebStateM () () ServerState) b
corsHeader = do
  ctx <- getContext
  setHeader "Access-Control-Allow-Origin" "*"
  pure ctx

app :: Api ()
app = prehook corsHeader $ do
  get "/api/courses" $ do
    db <- getState >>= (liftIO . readIORef . database)
    json $ courses db
  get "/api/course" $ do
    db <- getState >>= (liftIO . readIORef . database)
    cid <- param' "cid"
    json $ findCourse cid (courses db)
  post "/api/login/std" $ do
    stdAuth <- jsonBody' :: ApiAction StdAuth
    if isJust $ findStudent (stdId stdAuth)
      then json $ ApiResponse {message = "Login Successful", dataResponse = Nothing}
      else json $ ApiResponse {message = "Login Failed", dataResponse = Nothing}
  post "/api/courses" $ do
    newCourse <- jsonBody' :: ApiAction Course
    dbRef <- database <$> getState
    liftIO $
      atomicModifyIORef' dbRef $
        \db -> (Db {courses = courses db <> [newCourse], users = users db}, ())
    db <- getState >>= (liftIO . readIORef . database)
    liftIO $ I.writeFile "db.json" $ encodeToLazyText db
    json $ ApiResponse {message = "Course added", dataResponse = Just newCourse}
  delete "/api/courses/" $ do
    cid <- param' "cid"
    dbRef <- database <$> getState
    db <- getState >>= (liftIO . readIORef . database)
    liftIO $
      atomicModifyIORef' dbRef $
        \db -> (Db {courses = filter (\course -> courseId course /= cid) $ courses db, users = users db}, ())
    db' <- getState >>= (liftIO . readIORef . database)
    liftIO $ I.writeFile "db.json" $ encodeToLazyText db'
    if isJust (findCourse cid $ courses db)
      then json $ ApiResponse {message = "Course removed", dataResponse = findCourse cid $ courses db}
      else json $ ApiResponse {message = "No this course", dataResponse = Nothing}
  post "/api/courses/enroll" $ do
    cid <- param' "cid"
    secId <- param' "secId"
    studentId <- param' "studentId"
    dbRef <- database <$> getState
    db <- getState >>= (liftIO . readIORef . database)
    liftIO $
      atomicModifyIORef' dbRef $
        \db -> (Db {courses = enroll cid secId studentId $ courses db, users = users db}, ())
    db' <- getState >>= (liftIO . readIORef . database)
    liftIO $ I.writeFile "db.json" $ encodeToLazyText db'
    json $
      if isNothing $ findStudent studentId
        then ApiResponse {message = "No this student id", dataResponse = Nothing}
        else
          if alreadyEnroll studentId cid secId $ courses db
            then ApiResponse {message = "You have alrady enrolled this course", dataResponse = Nothing}
            else ApiResponse {message = "Enrolled", dataResponse = findCourse cid $ courses db'}
  post "/api/courses/drop" $ do
    cid <- param' "cid"
    secId <- param' "secId"
    studentId <- param' "studentId"
    dbRef <- database <$> getState
    db <- getState >>= (liftIO . readIORef . database)
    liftIO $
      atomicModifyIORef' dbRef $ \db ->
        (Db {courses = dropCourse cid secId studentId $ courses db, users = users db}, ())
    db' <- getState >>= (liftIO . readIORef . database)
    liftIO $ I.writeFile "db.json" $ encodeToLazyText db'
    json $
      if not $ alreadyEnroll studentId cid secId $ courses db
        then ApiResponse {message = "You haven't enroll this course", dataResponse = Nothing}
        else ApiResponse {message = "Droped", dataResponse = findCourse cid $ courses db'}

main :: IO ()
main = do
  db <- liftIO $ B.readFile "db.json"
  st <- ServerState <$> newIORef (decodeToJson db)
  spockCfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 8000 (spock spockCfg app)
