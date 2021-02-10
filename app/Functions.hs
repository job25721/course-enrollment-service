module Functions (allCourses, findCourse, enroll) where

import Data.Course (Course (..))

allCourses :: [Course]
allCourses =
  [ Course {courseId = 261497, name = "Functional Programming", credit = 3, seat = 10, enrolled = 3},
    Course {courseId = 261208, name = "Basic Computer Engr", credit = 3, seat = 100, enrolled = 29}
  ]

findCourse :: Int -> [Course] -> Maybe Course
findCourse _ [] = Nothing
findCourse cid (x : xs)
  | courseId x == cid = Just x
  | otherwise = findCourse cid xs

enroll :: Int -> [Course] -> [Course]
enroll cid = map (\course -> if courseId course == cid then Course {courseId = courseId course, name = name course, credit = credit course, seat = (-) (seat course) 1, enrolled = (+) (enrolled course) 1} else course)