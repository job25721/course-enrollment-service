module Store (allCourses, allStudents, allTeachers) where

import Data.Course (Course (..), Section (..))
import Data.Person (Student (..), Teacher (..), User (..))

getTeacherById :: Int -> Maybe Teacher
getTeacherById tid = foldl (\acc t -> if teacherId t == tid then Just t else acc) Nothing allTeachers

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x

allCourses :: [Course]
allCourses =
  [ Course
      { courseId = 261207,
        name = "Basic CPE Lab",
        credit = 3,
        lecturer = fromMaybe $ getTeacherById 2,
        sections =
          [Section {sectionId = 1, seat = 50, day = "M-Th", time = "11:00-12:30", enrolledPerson = [], room = "516"}]
      },
    Course
      { courseId = 261218,
        name = "Algorithms",
        credit = 3,
        lecturer = fromMaybe $ getTeacherById 1,
        sections =
          [ Section {sectionId = 1, seat = 50, day = "Tu-F", time = "14:30-16:00", enrolledPerson = [], room = "521"},
            Section {sectionId = 2, seat = 50, day = "Wed", time = "13:00-16:00", enrolledPerson = [], room = "422"}
          ]
      }
  ]

allTeachers :: [Teacher]
allTeachers =
  [ Teacher
      { teacherId = 1,
        teacherEmail = "chinawat.i@cmu.ac.th",
        teacherInfo =
          User
            { firstName = "Chinawat",
              lastName = "Isaradisaikul",
              userType = "teacher"
            }
      },
    Teacher
      { teacherId = 2,
        teacherEmail = "dome.p@cmu.ac.th",
        teacherInfo =
          User
            { firstName = "Dome",
              lastName = "Potikanond",
              userType = "teacher"
            }
      }
  ]

allStudents :: [Student]
allStudents =
  [ Student
      { studentId = 600610748,
        studentInfo =
          User
            { firstName = "Pathomporn",
              lastName = "Pankaew",
              userType = "student"
            },
        year = 4,
        academicYear = 2017,
        faculty = "Engineering",
        major = "Computer Engineering",
        advisor = "Professor Assistant Latchana Ramingwong"
      },
    Student
      { studentId = 600610749,
        studentInfo =
          User
            { firstName = "Parinya",
              lastName = "Seetawan",
              userType = "student"
            },
        year = 4,
        academicYear = 2017,
        faculty = "Engineering",
        major = "Computer Engineering",
        advisor = "Professor Assistant Latchana Ramingwong"
      }
  ]