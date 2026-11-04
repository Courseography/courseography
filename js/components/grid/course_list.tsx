import { useState, useEffect } from "react"
import { CourseEntry } from "./course_entry"

export interface CourseListProps {
  courseFilter: string
  selectedCourses: string[]
  selectCourse: (courseCode: string) => void
  removeCourse: (courseCode: string) => void
}

/**
 * A list of courses in column flow
 * Composed of many CourseEntry
 */

export function CourseList({
  courseFilter,
  selectedCourses,
  selectCourse,
  removeCourse,
}: CourseListProps) {
  const [courses, setCourses] = useState<string[]>([])

  useEffect(() => {
    // AJAX requests allow the programmer to:
    //    1. update a webpage without refreshing
    //    2. Request data from a server AFTER the webpage is loaded
    //    3. Send data to the server - in the background

    // This makes an AJAX call to retrieve courses from the database
    fetch(
      "/courses" // url to which the AJAX request is sent to
    )
      .then(response => response.text())
      .then(data => {
        // searches through all of the courses in "data",
        // and stores each individual course code name
        // into 'courses' list
        const courses = data.split("\n").map(course => course.substring(0, 8))
        setCourses(courses)
      })
  }, [])

  let searchList: React.ReactElement[] = []
  // If there are courses to be filtered
  if (courseFilter !== "") {
    // From the "courses" list, filter out elements based off of the prop "courseFilter" passed to
    // CourseList by SearchPanel
    searchList = courses
      .filter(course => course.indexOf(courseFilter) > -1)
      .map(course => (
        <CourseEntry
          course={course}
          key={course}
          selectCourse={selectCourse}
          removeCourse={removeCourse}
          selectedCourses={selectedCourses}
        />
      ))
  }

  // Return all the unfiltered courses in the "courses" list in a list
  return (
    <div id="search-list">
      <ul>{searchList}</ul>
    </div>
  )
}
