module Store (allCourses, students) where

import Data.Course (Course (..), Section (..))
import Data.Person (Student (..))

allCourses :: [Course]
allCourses =
  [ Course
      { courseId = 261497,
        name = "Functional Programming",
        credit = 3,
        lecturer = "Chin Isradisaikul",
        sections =
          [Section {sectionId = 1, seat = 50, day = "M-Th", time = "11:00-12:30", enrolledPerson = []}]
      },
    Course
      { courseId = 261208,
        name = "Algorithms",
        credit = 3,
        lecturer = "Chin Isradisaikul",
        sections =
          [ Section {sectionId = 1, seat = 50, day = "Tu-F", time = "14:30-16:00", enrolledPerson = []},
            Section {sectionId = 2, seat = 50, day = "Wed", time = "13:00-16:00", enrolledPerson = []}
          ]
      }
  ]

students :: [Student]
students =
  [ Student
      { studentId = 600610748,
        firstName = "Pathomporn",
        lastName = "Pankaew",
        year = 4,
        academicYear = 2017,
        faculty = "Engineering",
        major = "Computer Engineering",
        advisor = "Professor Assistant Latchana Ramingwong"
      },
    Student
      { studentId = 600610749,
        firstName = "Parinya",
        lastName = "Seetawan",
        year = 4,
        academicYear = 2017,
        faculty = "Engineering",
        major = "Computer Engineering",
        advisor = "Professor Assistant Latchana Ramingwong"
      }
  ]