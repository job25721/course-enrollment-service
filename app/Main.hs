{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Course (Course (..))
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import Data.Text (pack)
import Data.Text.Lazy.IO as I (writeFile)
import Functions (alreadyEnroll, dropCourse, enroll, findCourse)
import Store (allCourses)
import Web.Spock
import Web.Spock.Config
  ( PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
  )

newtype ServerState = ServerState {courses :: IORef [Course]}

type Api a = SpockM () () ServerState a

type ApiAction a = SpockAction () () ServerState a

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x

resetFile :: IO ()
resetFile = I.writeFile "courses.json" (encodeToLazyText allCourses)

decodeCoursesArray :: B.ByteString -> [Course]
decodeCoursesArray courses' = fromMaybe (decode courses' :: Maybe [Course])

corsHeader :: ActionCtxT b (WebStateM () () ServerState) b
corsHeader = do
  ctx <- getContext
  setHeader "Access-Control-Allow-Origin" "*"
  pure ctx

app :: Api ()
app = prehook corsHeader $ do
  get "/api/courses" $ do
    courses' <- getState >>= (liftIO . readIORef . courses)
    json courses'
  get "/api/course" $ do
    courses' <- getState >>= (liftIO . readIORef . courses)
    cid <- param' "cid"
    json $ findCourse cid courses'
  post "/api/courses" $ do
    newCourse <- jsonBody' :: ApiAction Course
    coursesRef <- courses <$> getState
    liftIO $
      atomicModifyIORef' coursesRef $ \courses ->
        (courses ++ [newCourse], ())
    courses' <- getState >>= (liftIO . readIORef . courses)
    liftIO $ I.writeFile "courses.json" $ encodeToLazyText courses'
    text $ "added" <> pack (show newCourse)
  delete "/api/courses" $ do
    cid <- param' "cid"
    coursesRef <- courses <$> getState
    liftIO $
      atomicModifyIORef' coursesRef $ \courses ->
        (filter (\course -> courseId course /= cid) courses, ())
    courses' <- getState >>= (liftIO . readIORef . courses)
    liftIO $ I.writeFile "courses.json" $ encodeToLazyText courses'
    json $ "removed " <> show cid
  post "/api/courses/enroll" $ do
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
        then show "already enrolled"
        else show $ findCourse cid courses''
  post "/api/courses/drop" $ do
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
  runSpock 8000 (spock spockCfg app)
