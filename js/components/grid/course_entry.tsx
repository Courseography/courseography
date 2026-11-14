import { useCallback } from "react"

export interface CourseEntryProps {
  course: string
  selectCourse: (courseCode: string) => void
  removeCourse: (courseCode: string) => void
  selectedCourses: string[]
}

/**
 * Describes a course based on its course code, and whether or not it has been selected
 * (If the course is selected, it is a "starred-course").
 */

export function CourseEntry({
  course,
  selectCourse,
  removeCourse,
  selectedCourses,
}: CourseEntryProps) {
  // Check whether the course is already in the selectCourses list.
  // Remove the course if it is, or add the course if it is not.
  const select = useCallback(() => {
    if (selectedCourses.indexOf(course) != -1) {
      removeCourse(course)
    } else {
      selectCourse(course)
    }
  }, [course, selectedCourses, selectCourse, removeCourse])

  const classes = selectedCourses.indexOf(course) != -1 ? "starred-course" : ""
  return (
    <li id={course + "-search"} className={classes} onClick={select}>
      {course}
    </li>
  )
}
