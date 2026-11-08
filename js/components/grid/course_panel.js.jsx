import { useCallback, useState } from "react"
import { CourseModal } from "../common/CourseModal"
import { CourseList } from "./course_list"
import { Course } from "./course"

/**
 * Creates a search box and stores the current user input that is in the search box
 * Holds courses selected from the search box, and lists of their F, S and Y lecture, tutorial,
 * and practical sections.
 */
export function CoursePanel(props) {
  const [value, setValue] = useState("")
  const [courseInfoId, setCourseInfoId] = useState(null) // The course code to display in the CourseModal

  const handleInput = useCallback(event => {
    setValue(event.target.value)
  }, [])

  // Only clear all selected courses if the user confirms in the alert
  // pop up window.
  const clearAllCourses = useCallback(() => {
    if (window.confirm("Clear all selected courses?")) {
      props.clearCourses()
    }
  }, [props.clearCourses])

  const displayInfo = useCallback(courseId => {
    setCourseInfoId(courseId)
  }, [])

  const courses = props.selectedCourses.map(course => (
    <Course
      key={course}
      selectedLectures={props.selectedLectures}
      courseCode={course}
      removeCourse={props.removeCourse}
      hoverLecture={props.hoverLecture}
      unhoverLecture={props.unhoverLecture}
      selectLecture={props.selectLecture}
      displayInfo={displayInfo}
    />
  ))

  return (
    <div id="search-layout" className="col-md-2 col-12">
      <div id="filter-container">
        <form onSubmit={() => false}>
          <input
            id="course-filter"
            className="form-control"
            placeholder="Enter a course!"
            autoComplete="off"
            type="text"
            value={value}
            onChange={handleInput}
          />
        </form>
      </div>
      <div id="search-container">
        <CourseList
          courseFilter={value.toUpperCase()}
          selectedCourses={props.selectedCourses}
          selectCourse={props.selectCourse}
          removeCourse={props.removeCourse}
        />
      </div>
      <div id="course-select-wrapper">
        <ul className="trapScroll-enabled" id="course-select">
          <li id="clear-all" key="clear-all-grid" onClick={clearAllCourses}>
            <h3>Clear All</h3>
          </li>
          {courses}
        </ul>
      </div>
      <CourseModal
        showCourseModal={!!courseInfoId}
        courseId={courseInfoId}
        onClose={() => setCourseInfoId(null)}
      />
    </div>
  )
}
