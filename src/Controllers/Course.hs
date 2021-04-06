{-# LANGUAGE OverloadedStrings #-}

module Controllers.Course (getCourse, getCourseById, addNewCourse, deleteCourse, enrollCourse, dropCourseById) where

import Control.Monad.IO.Class
import Data.Aeson.Text (encodeToLazyText)
import Data.Course
import Data.Database
import Data.IORef
import Data.ReturnApi
import Data.Text.Lazy.IO as I
import Functions (alreadyEnroll, dropCourse, enroll, findCourse, findStudent, isJust, isNothing)
import Types
import Web.Spock

getCourse :: Controller
getCourse = do
  db <- getState >>= (liftIO . readIORef . database)
  json $ courses db

getCourseById :: Controller
getCourseById = do
  db <- getState >>= (liftIO . readIORef . database)
  cid <- param' "cid"
  json $ findCourse cid (courses db)

addNewCourse :: Controller
addNewCourse = do
  newCourse <- jsonBody' :: ApiAction Course
  dbRef <- database <$> getState
  liftIO $
    atomicModifyIORef' dbRef $
      \db -> (Db {courses = courses db <> [newCourse], users = users db}, ())
  db <- getState >>= (liftIO . readIORef . database)
  liftIO $ I.writeFile "db.json" $ encodeToLazyText db
  json $ ApiResponse {message = "Course added", dataResponse = Just newCourse}

deleteCourse :: Controller
deleteCourse = do
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

enrollCourse :: Controller
enrollCourse = do
  cid <- param' "cid"
  secId <- param' "secId"
  sid <- param' "sid"
  dbRef <- database <$> getState
  db <- getState >>= (liftIO . readIORef . database)
  if isJust $ findCourse cid (courses db)
    then liftIO $
      atomicModifyIORef' dbRef $
        \db ->
          (Db {courses = enroll cid secId sid $ courses db, users = users db}, ())
    else json $ ApiResponse {message = "no this course", dataResponse = Nothing}
  db' <- getState >>= (liftIO . readIORef . database)
  liftIO $ I.writeFile "db.json" $ encodeToLazyText db'
  json $
    if isNothing $ findStudent sid
      then ApiResponse {message = "No this student id", dataResponse = Nothing}
      else
        if alreadyEnroll sid cid secId $ courses db
          then ApiResponse {message = "You has already enrolled this course", dataResponse = Nothing}
          else ApiResponse {message = "Enrolled", dataResponse = findCourse cid $ courses db'}

dropCourseById :: Controller
dropCourseById = do
  cid <- param' "cid"
  secId <- param' "secId"
  sid <- param' "sid"
  dbRef <- database <$> getState
  db <- getState >>= (liftIO . readIORef . database)
  liftIO $
    atomicModifyIORef' dbRef $ \db ->
      (Db {courses = dropCourse cid secId sid $ courses db, users = users db}, ())
  db' <- getState >>= (liftIO . readIORef . database)
  liftIO $ I.writeFile "db.json" $ encodeToLazyText db'
  json $
    if not $ alreadyEnroll sid cid secId $ courses db
      then ApiResponse {message = "You haven't enroll this course", dataResponse = Nothing}
      else ApiResponse {message = "Dropped", dataResponse = findCourse cid $ courses db'}