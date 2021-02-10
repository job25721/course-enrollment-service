# course-enrollment-service

### API DOCS

### `baseURL : /api` <br>

`GET /courses` : Get all courses <br>
`GET /course?cid=Int` : Get course by course id <br>
`POST /courses/add` : Add new course <br>

<pre>body
{
    courseId : Int
    name : String
    credit : Int
    seat : Int
    enrolled : Int
}</pre>

`GET /enroll?cid=Int` : Enroll a course by id
