import { useCallback, useState } from "react"
import { CourseModal } from "../common/CourseModal"
import { CourseList } from "./course_list"
import { Course } from "./course"
import { SelectedLecture } from "./types"

export interface CoursePanelProps {
  selectedCourses: string[]
  selectedLectures: SelectedLecture[]
  removeCourse: (courseCode: string) => void
  clearCourses: () => void
  hoverLecture: (lecture: SelectedLecture) => void
  unhoverLecture: () => void
  selectLecture: (lecture: SelectedLecture) => void
  selectCourse: (courseCode: string) => void
}

/**
 * Creates a search box and stores the current user input that is in the search box
 * Holds courses selected from the search box, and lists of their F, S and Y lecture, tutorial,
 * and practical sections.
 */
export function CoursePanel({
  selectedCourses,
  selectedLectures,
  removeCourse,
  clearCourses,
  hoverLecture,
  unhoverLecture,
  selectLecture,
  selectCourse,
}: CoursePanelProps) {
  const [value, setValue] = useState("")
  const [courseInfoId, setCourseInfoId] = useState<string | undefined>(undefined) // The course code to display in the CourseModal

  const handleInput = useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    setValue(event.target.value)
  }, [])

  // Only clear all selected courses if the user confirms in the alert
  // pop up window.
  const clearAllCourses = useCallback(() => {
    if (window.confirm("Clear all selected courses?")) {
      clearCourses()
    }
  }, [clearCourses])

  const displayInfo = useCallback((courseId: string) => {
    setCourseInfoId(courseId)
  }, [])

  const courses = selectedCourses.map(course => (
    <Course
      key={course}
      selectedLectures={selectedLectures}
      courseCode={course}
      removeCourse={removeCourse}
      hoverLecture={hoverLecture}
      unhoverLecture={unhoverLecture}
      selectLecture={selectLecture}
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
          selectedCourses={selectedCourses}
          selectCourse={selectCourse}
          removeCourse={removeCourse}
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
        onClose={() => setCourseInfoId(undefined)}
      />
    </div>
  )
}
