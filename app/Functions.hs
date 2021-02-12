module Functions (findCourse, enroll, alreadyEnroll, dropCourse) where

import Data.Course (Course (..), Section (..))
import Data.Person (Student (..))
import Store

fromMaybe (Just x) = x

findCourse :: Int -> [Course] -> Maybe Course
findCourse _ [] = Nothing
findCourse cid (x : xs)
  | courseId x == cid = Just x
  | otherwise = findCourse cid xs

findSection :: Int -> [Section] -> Maybe Section
findSection _ [] = Nothing
findSection secId (x : xs)
  | sectionId x == secId = Just x
  | otherwise = findSection secId xs

findStudent :: Int -> [Student] -> Maybe Student
findStudent _ [] = Nothing
findStudent sid (x : xs)
  | studentId x == sid = Just x
  | otherwise = findStudent sid xs

alreadyEnroll :: Int -> Int -> Int -> [Course] -> Bool
alreadyEnroll sid cid secId courses
  | findStudent sid (enrolledPerson (fromMaybe (findSection secId (sections (fromMaybe (findCourse cid courses)))))) == Nothing = False
  | otherwise = True

enroll :: Int -> Int -> Int -> [Course] -> [Course]
enroll cid secId sid courses
  | findStudent sid students /= Nothing && alreadyEnroll sid cid secId courses == False =
    map
      ( \course ->
          if courseId course == cid
            then
              Course
                { courseId = courseId course,
                  name = name course,
                  credit = credit course,
                  lecturer = lecturer course,
                  sections =
                    map
                      ( \sec ->
                          if sectionId sec == secId
                            then
                              Section
                                { sectionId = sectionId sec,
                                  seat = seat sec - 1,
                                  enrolledPerson = fromMaybe (findStudent sid students) : enrolledPerson sec,
                                  time = time sec,
                                  day = day sec
                                }
                            else sec
                      )
                      (sections course)
                }
            else course
      )
      courses
  | otherwise = courses

dropCourse :: Int -> Int -> Int -> [Course] -> [Course]
dropCourse cid secId sid =
  map
    ( \course ->
        if courseId course == cid
          then
            Course
              { courseId = courseId course,
                name = name course,
                credit = credit course,
                lecturer = lecturer course,
                sections =
                  map
                    ( \sec ->
                        if sectionId sec == secId
                          then
                            Section
                              { sectionId = sectionId sec,
                                seat = seat sec + 1,
                                enrolledPerson = filter (\p -> studentId p /= sid) (enrolledPerson sec),
                                time = time sec,
                                day = day sec
                              }
                          else sec
                    )
                    (sections course)
              }
          else course
    )
