# course-enrollment-service

<u>Functional programming final-project</u>
<br>
created by Spock framework https://www.spock.li/

### API DOCS

### `baseURL : /api` <br>

`GET /courses` : Get all courses <br>
`GET /course?cid=Int` : Get course by course id <br>
`POST /courses/add` : Add new course <br>

```js
body
{
    "courseId" : Number,
    "name" : String,
    "credit" : Number,
    "lecturer" : String,
    "sections" :
    [
        {
            "sectionId" : Number,
            "seat" : Number,
            "enrolledPerson" : []
        }
    ]
}
```

`POST /enroll?cid=Int&secId=Int&sid=Int` : Enroll a course by id secId and studentId
<br>
`POST /drop?cid=Int&secId=Int&sid=Int` : Drop a course by id secId and studentId
