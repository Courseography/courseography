import React, { useCallback, useState, useEffect } from "react"
import { CourseModal } from "../common/react_modal.js.jsx"
import { getCourse } from "../common/utils"

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

/**
 * A selected course with a delete button, and info button, which displays information
 * about the course when clicked.
 * Also retrieves the course information. It holds list of F, S and Y lecture, tutorial
 * and practical sections for the course.
 */
function Course(props) {
  const [selected, setSelected] = useState(false)
  const [courseInfo, setCourseInfo] = useState({})

  const filterLectureList = useCallback((lectures, session) => {
    return lectures
      .filter(lec => lec.session === session)
      .sort((firstLec, secondLec) =>
        firstLec.lectureCode > secondLec.lectureCode ? 1 : -1
      )
  }, [])

  const toggleSelect = useCallback(() => {
    setSelected(s => !s)
  }, [])

  const removeCourse = useCallback(() => {
    props.removeCourse(props.courseCode)
  }, [props.removeCourse, props.courseCode])

  const parseLectures = useCallback(lectures => {
    const parsedLectures = []

    // Loop through the lecture sections to get each section's session code and lecture times
    lectures.forEach(lectureInfo => {
      // Check to make sure its not an online section (online sections have course codes beginning with 9) or
      // restricted section. Restricted sections have enrollment restricted for a particular group of students,
      // but happens at the same time and place as a regular lecture/tutorial section.
      if (
        lectureInfo.meetData.section.charAt(3) !== "2" &&
        lectureInfo.meetData.section.charAt(3) !== "9"
      ) {
        const lecture = {
          courseCode:
            lectureInfo.meetData.code +
            " (" +
            lectureInfo.meetData.section.substring(0, 1) +
            ")",
          lectureCode:
            lectureInfo.meetData.section.substring(0, 1) +
            lectureInfo.meetData.section.substring(3),
          session: lectureInfo.meetData.session,
          times: lectureInfo.timeData,
        }
        parsedLectures.push(lecture)
      }
    })
    return parsedLectures
  }, [])

  const containsSelectedLecture = useCallback(() => {
    // Only use method subString on the value of courseInfo.courseCode if
    // if the courseInfo.courseCode exists (ie the course information has already been fetched)
    if (courseInfo.courseCode) {
      const lectures = props.selectedLectures.map(lecture =>
        lecture.courseCode.substring(0, 6)
      )
      console.log(lectures) // TODO remove
      const courseCode = courseInfo.courseCode
      return lectures.indexOf(courseCode.substring(0, 6)) >= 0
    }
    return false
  }, [props.selectedLectures])

  useEffect(() => {
    getCourse(props.courseCode).then(data => {
      const course = {
        courseCode: "",
        F: [],
        S: [],
        Y: [],
      }
      course.courseCode = data.name

      const parsedLectures = parseLectures(data.allMeetingTimes)
      // Split the lecture sections into Fall, Spring and Years
      course.F = filterLectureList(parsedLectures, "F")
      course.S = filterLectureList(parsedLectures, "S")
      course.Y = filterLectureList(parsedLectures, "Y")
      setCourseInfo(course)
    })
  }, [])

  return (
    <li
      key={props.courseCode}
      id={props.courseCode + "-li"}
      className={"ui-accordion ui-widget ui-helper-reset"}
    >
      <div
        className="ui-accordion-header ui-helper-reset ui-state-default ui-accordion-icons ui-accordion-header-active ui-state-active ui-corner-top"
        id={"ui-accordion-" + props.courseCode + "-li-header-0"}
      >
        <div className="icon-div">
          <img
            src="/static/res/ico/delete.png"
            className="close-icon"
            onClick={removeCourse}
          />
          <img
            src="/static/res/ico/about.png"
            className="close-icon"
            onClick={() => props.displayInfo(props.courseCode)}
          />
        </div>
        <h3
          onClick={toggleSelect}
          data-satisfied="true"
          taken={containsSelectedLecture() ? "true" : "false"}
        >
          {props.courseCode}
        </h3>
      </div>
      {selected && (
        <div
          className="sections ui-accordion-content ui-helper-reset ui-widget-content ui-corner-bottom ui-accordion-content-active"
          id={"ui-accordion-" + props.courseCode + "-li-panel-0"}
        >
          <SectionList
            courseCode={props.courseCode}
            session="Y"
            lectures={courseInfo.Y}
            hoverLecture={props.hoverLecture}
            unhoverLecture={props.unhoverLecture}
            selectLecture={props.selectLecture}
          />
          <SectionList
            courseCode={props.courseCode}
            session="F"
            lectures={courseInfo.F}
            hoverLecture={props.hoverLecture}
            unhoverLecture={props.unhoverLecture}
            selectLecture={props.selectLecture}
          />
          <SectionList
            courseCode={props.courseCode}
            session="S"
            lectures={courseInfo.S}
            hoverLecture={props.hoverLecture}
            unhoverLecture={props.unhoverLecture}
            selectLecture={props.selectLecture}
          />
        </div>
      )}
    </li>
  )
}

/**
 * A list of lecture, tutorial and practical sections for the specified course for the specified
 * session.
 */
function SectionList(props) {
  console.log(props)
  const lectureSections = props.lectures.map(lecture => (
    <LectureSection
      key={props.courseCode + lecture.lectureCode + props.section}
      session={props.session}
      courseCode={props.courseCode}
      lecture={lecture}
      hoverLecture={props.hoverLecture}
      unhoverLecture={props.unhoverLecture}
      selectLecture={props.selectLecture}
    />
  ))
  return (
    <ul className={"sectionList-" + props.session} id="lecture-list">
      {lectureSections}
    </ul>
  )
}

/**
 * A lecture, tutorial or practical section for the specified course in the specified
 * session. The section is added to or removed from a list of selected lecture upon click.
 */
const LectureSection = props => {
  return (
    <li
      id={props.courseCode + "-" + props.lecture.lectureCode + "-" + props.session}
      onClick={() => props.selectLecture(props.lecture)}
      onMouseOver={() => props.hoverLecture(props.lecture)}
      onMouseOut={() => props.unhoverLecture()}
      data-satisfied={"true"}
    >
      {props.lecture.lectureCode}
    </li>
  )
}

class CourseList extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      courses: [],
    }
  }

  componentDidMount() {
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
        this.setState({ courses: courses })
      })
  }

  render() {
    let searchList = []
    // If there are courses to be filtered
    if (this.props.courseFilter !== "") {
      // From the "courses" list, filter out elements based off of the prop "courseFilter" passed to
      // CourseList by SearchPanel
      searchList = this.state.courses
        .filter(course => course.indexOf(this.props.courseFilter) > -1)
        .map(course => (
          <CourseEntry
            course={course}
            key={course}
            selectCourse={this.props.selectCourse}
            removeCourse={this.props.removeCourse}
            selectedCourses={this.props.selectedCourses}
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
}

/**
 * Describes a course based on its course code, and whether or not it has been selected
 * (If the course is selected, it is a "starred-course").
 */
class CourseEntry extends React.Component {
  constructor(props) {
    super(props)
    this.select = this.select.bind(this)
  }

  // Check whether the course is already in the selectCourses list.
  // Remove the course if it is, or add the course if it is not.
  select() {
    if (this.props.selectedCourses.indexOf(this.props.course) != -1) {
      this.props.removeCourse(this.props.course)
    } else {
      this.props.selectCourse(this.props.course)
    }
  }

  render() {
    const classes =
      this.props.selectedCourses.indexOf(this.props.course) != -1
        ? "starred-course"
        : ""
    return (
      <li id={this.props.course + "-search"} className={classes} onClick={this.select}>
        {this.props.course}
      </li>
    )
  }
}
