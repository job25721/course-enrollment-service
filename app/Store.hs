module Store (allCourses, students) where

import Data.Course (Course (..), Section (..))
import Data.Person (Student (..))

allCourses :: [Course]
allCourses =
  [ Course
      { courseId = 261497,
        name = "Functional Programming",
        credit = 3,
        sections =
          [Section {sectionId = 1, seat = 50, enrolledPerson = []}]
      },
    Course
      { courseId = 261208,
        name = "Basic Computer Engr",
        credit = 3,
        sections =
          [ Section {sectionId = 1, seat = 50, enrolledPerson = []},
            Section
              { sectionId = 2,
                seat = 50,
                enrolledPerson =
                  [ Student
                      { studentId = 600610748,
                        firstName = "Pathomporn",
                        lastName = "Pankaew",
                        year = 4,
                        academicYear = 2017,
                        faculty = "Engineering",
                        major = "Computer Engineering",
                        advisor = "Professor Assistant Latchana Ramingwong"
                      }
                  ]
              }
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