{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controllers.Course (addNewCourse, deleteCourse, dropCourseById, enrollCourse, getCourse, getCourseById)
import Controllers.Student (getMyEnrolledCourse, getStdInfo, studentLogin)
import Controllers.Teacher (getTeacherAddedCourses, getTeacherInfo, teacherLogin)
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
import Data.Person (Users (..))
import Data.Text.Lazy.IO as I (writeFile)
import Functions (fromMaybe)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Store (allCourses, allStudents, allTeachers)
import Types (ServerState (..))
import Web.Spock
import Web.Spock.Config
  ( PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
  )

resetFile :: IO ()
resetFile =
  I.writeFile "db.json" $
    encodeToLazyText Db {courses = allCourses, users = Users {students = allStudents, teachers = allTeachers}}

app :: SpockM () () ServerState ()
app = do
  get "/api/courses" getCourse
  get "/api/course" getCourseById
  post "/api/courses" addNewCourse
  delete "/api/courses" deleteCourse
  post "/api/user/std/login" studentLogin
  get "/api/user/std/my" getStdInfo
  get "/api/user/std/courses" getMyEnrolledCourse
  post "/api/user/std/enroll" enrollCourse
  post "/api/user/std/drop" dropCourseById
  post "/api/user/teacher/login" teacherLogin
  get "/api/user/teacher/my" getTeacherInfo
  get "/api/user/teacher/courses" getTeacherAddedCourses

decodeToJson :: B.ByteString -> Db
decodeToJson db = fromMaybe (decode db :: Maybe Db)

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just (simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}))

main :: IO ()
main = do
  db <- B.readFile "db.json"
  state <- ServerState <$> newIORef (decodeToJson db)
  spockCfg <- defaultSpockCfg () PCNoDatabase state
  myApp <- spockAsApp (spock spockCfg app)
  run 8000 (corsMiddleware myApp)
