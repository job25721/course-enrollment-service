const fs = require("fs");
const courses = [];
const day = ["M-Th", "Tu-F", "Wed", "Tue", "Fri", "Mon", "Thu", "Sat"];
const time = [
  "08:00-9:30",
  "09:30-11:00",
  "11:00-12:30",
  "13:00-14:30",
  "14:30-16:00",
];
for (let i = 1; i <= 497; i++) {
  courses.push({
    courseId: parseInt(`261${i < 10 ? `00${i}` : i < 100 ? `0${i}` : i}`),
    name: "Dummy",
    credit: Math.floor(Math.random() * 3) + 1,
    lecturer: "Dummy",
    sections: [],
  });
  const secLen = Math.floor(Math.random() * 3) + 1;

  for (let j = 0; j < secLen; j++) {
    courses[i - 1].sections.push({
      sectionId: j + 1,
      seat: Math.floor(Math.random() * 90) + 10,
      enrolledPerson: [],
      day: day[Math.floor(Math.random() * day.length) + 1],
      time: time[Math.floor(Math.random() * time.length) + 1],
    });
  }
}

fs.writeFileSync("./courses.json", JSON.stringify(courses), {
  encoding: "ascii",
});
