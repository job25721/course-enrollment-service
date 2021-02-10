# course-enrollment-service

created by Spock framework https://www.spock.li/

### API DOCS

### `baseURL : /api` <br>

`GET /courses` : Get all courses <br>
`GET /course?cid=Int` : Get course by course id <br>
`POST /courses/add` : Add new course <br>

```js
body
{
    "courseId" : Number
    "name" : String
    "credit" : Number
    "seat" : Number
    "enrolled" : Number
}
```

`GET /enroll?cid=Int` : Enroll a course by id
