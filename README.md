# course-enrollment-service

<u>Functional programming final-project</u>
<br>
created by Spock framework https://www.spock.li/

### API DOCS

### `baseURL : {hostname}/api` <br>

`GET /courses` : Get all courses <br>
`GET /course?cid=Int` : Get course by course id <br>
`POST /courses` : Add new course <br>

```hs
{
    "courseId" : Int,
    "name" : String,
    "credit" : Int,
    "lecturer" : Teacher,
    "sections" :
    [
        {
            "sectionId" : Int,
            "seat" : Int,
            "enrolledPerson" : []
        }
    ]
}
```

`POST /course/delete` : Delete course <br>

```hs
@param
cid : Int
```

`POST /user/std/enroll` : Enroll a course by id secId and studentId

```hs
@param
cid : Int
sedId : Int
sid : Int
```

`POST /user/std/drop` : Drop a course by id secId and studentId

```hs
@param
cid : Int
sedId : Int
sid : Int
```

`POST /user/std/login` : Student login

```hs
{
    stdId : Int
}
```

`GET /user/std/ny` : Get my info (student)

```hs
@param
sid : String
```

`GET /user/std/courses` : Get my enrolled course

```hs
@param
sid : Int
```

`POST /user/teacher/login` : Teacher login

```hs
{
    tEmail : String
}
```

`GET /user/teacher/ny` : Get my info (teacher)

```hs
@param
email : String
```
