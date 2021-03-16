module Functions (findCourse, enroll, alreadyEnroll, dropCourse, isJust, isNothing, findStudent, findMyEnrolledCourse, findTeacher, findMyAddedCourse, fromMaybe) where

import Data.Course (Course (..), Section (..))
import Data.Person (Student (..), Teacher (..))
import Store (allStudents, allTeachers)

fromMaybe :: Maybe a -> a
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

findStudent :: Int -> Maybe Student
findStudent sid = foldl (\acc student -> if sid == studentId student then Just student else acc) Nothing allStudents

findTeacher :: [Char] -> Maybe Teacher
findTeacher email = foldl (\acc teacher -> if email == teacherEmail teacher then Just teacher else acc) Nothing allTeachers

findMyAddedCourse :: [Char] -> [Course] -> [Course]
findMyAddedCourse email = filter (\course -> teacherEmail (lecturer course) == email)

findEnrolledStd :: Int -> [Student] -> Bool
findEnrolledStd sid = foldl (\acc cur -> (studentId cur == sid) || acc) False

findMyEnrollSection :: Int -> [Section] -> Bool
findMyEnrollSection sid = foldl (\acc cur -> findEnrolledStd sid (enrolledPerson cur) || acc) False

findMyEnrolledCourse :: Int -> [Course] -> [Course]
findMyEnrolledCourse sid = filter (findMyEnrollSection sid . sections)

alreadyEnroll :: Int -> Int -> Int -> [Course] -> Bool
alreadyEnroll sid cid secId courses = findEnrolledStd sid $ enrolledPerson (fromMaybe (findSection secId (sections (fromMaybe (findCourse cid courses)))))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

enroll :: Int -> Int -> Int -> [Course] -> [Course]
enroll cid secId sid courses
  | isJust (findStudent sid) && not (alreadyEnroll sid cid secId courses) =
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
                          if sectionId sec == secId && seat sec /= 0
                            then
                              Section
                                { sectionId = sectionId sec,
                                  seat = seat sec - 1,
                                  enrolledPerson = fromMaybe (findStudent sid) : enrolledPerson sec,
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
dropCourse cid secId sid courses
  | isJust (findStudent sid) && alreadyEnroll sid cid secId courses =
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
      courses
  | otherwise = courses