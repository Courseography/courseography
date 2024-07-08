import React from "react"
import {createRoot} from "react-dom/client"

import { CoursePanel } from "./course_panel.js.jsx"
import { Row } from "./calendar.js.jsx"
import { ExportModal } from "../common/export.js.jsx"
import Disclaimer from "../common/Disclaimer"

/**
 * Renders the course panel, the Fall and Spring timetable grids and search panel.
 * Also keeps track of all the selected courses and lectures.
 */
class Grid extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      selectedLectures: [],
      selectedCourses: [],
      hoveredLecture: null,
    }

    this.selectLecture = this.selectLecture.bind(this)
    this.addSelectedCourse = this.addSelectedCourse.bind(this)
    this.removeSelectedCourse = this.removeSelectedCourse.bind(this)
    this.clearSelectedCourses = this.clearSelectedCourses.bind(this)

    this.addSelectedLecture = this.addSelectedLecture.bind(this)
    this.removeSelectedLecture = this.removeSelectedLecture.bind(this)
    this.isSelectedLecture = this.isSelectedLecture.bind(this)

    this.hoverLecture = this.hoverLecture.bind(this)
    this.unhoverLecture = this.unhoverLecture.bind(this)
    this.setExportRef = this.setExportRef.bind(this)
  }

  // get the previously selected courses and lecture sections from local storage
  componentDidMount() {
    let selectedCoursesLocalStorage = localStorage.getItem("selectedCourses")
    let selectedLecturesLocalStorage = localStorage.getItem("selectedLectures")

    if (!selectedLecturesLocalStorage) {
      selectedLecturesLocalStorage = []
    } else {
      try {
        this.setState({
          selectedLectures: JSON.parse(selectedLecturesLocalStorage),
        })
      } catch (e) {
        console.log(e)
      }
    }

    if (!selectedCoursesLocalStorage) {
      selectedCoursesLocalStorage = []
    } else {
      selectedCoursesLocalStorage = selectedCoursesLocalStorage.split("_")
      const selectedCourses = []
      selectedCoursesLocalStorage.forEach(courseCode => {
        // Not using this.addSelectedCourse(courseCode) because each time addSelectedCourse is
        // called, this.setState is used.
        // setState is asynchronous and calling it several times in a row can lead to bugs when
        // new state depends on previous state
        selectedCourses.push(courseCode)
      })
      this.setState({ selectedCourses: selectedCourses })
    }

    // Enable "Export" link
    document.getElementById("nav-export")?.addEventListener("click", () => {
      this.exportModal.openModal()
    })
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.selectedCourses !== prevState.selectedCourses) {
      localStorage.setItem("selectedCourses", this.state.selectedCourses.join("_"))
    }
    if (this.state.selectedLectures !== prevState.selectedLectures) {
      localStorage.setItem(
        "selectedLectures",
        JSON.stringify(this.state.selectedLectures)
      )
    }
  }

  // Method passed to child component SearchPanel to add a course to selectedCourses.
  addSelectedCourse(courseCode) {
    // updatedCourses is a copy of this.state.selectedCourses so that prevState can be distinguished from
    // the current state in lifecycle methods like componentDidUpdate
    const updatedCourses = this.state.selectedCourses.slice()
    updatedCourses.push(courseCode)
    this.setState({ selectedCourses: updatedCourses })
  }

  // Method passed to child components, SearchPanel and CoursePanel to remove a course from selectedCourses.
  removeSelectedCourse(courseCode) {
    const updatedCourses = this.state.selectedCourses.slice()
    const index = updatedCourses.indexOf(courseCode)
    updatedCourses.splice(index, 1)
    this.setState({ selectedCourses: updatedCourses })

    const updatedLectures = this.state.selectedLectures.filter(
      lecture => !lecture.courseCode.includes(courseCode)
    )
    this.setState({ selectedLectures: updatedLectures })
  }

  // Method passed to child component CoursePanel to clear all the courses in selectedCourses.
  clearSelectedCourses() {
    this.setState({
      selectedCourses: [],
      selectedLectures: [],
    })
  }

  // Method passed to child component CoursePanel to add a lecture to selectedLectures
  addSelectedLecture(newLecture) {
    // The maximum number of courses in the lecture list with the same code is 3, one for each session (F, S, Y)
    const updatedLectures = this.state.selectedLectures.filter(lecture => {
      return (
        lecture.courseCode !== newLecture.courseCode ||
        lecture.session !== newLecture.session
      )
    })
    updatedLectures.push(newLecture)
    this.setState({ selectedLectures: updatedLectures })
  }

  // Method passed to child component CoursePanel to remove a lecture from selectedLectures
  removeSelectedLecture(courseCode, session) {
    const updatedLectures = this.state.selectedLectures.filter(lecture => {
      return lecture.courseCode !== courseCode || lecture.session !== session
    })
    this.setState({ selectedLectures: updatedLectures })
  }

  // Remove the lecture if it is already in the selectedLectures list, or add the lecture if it is not.
  selectLecture(lecture) {
    if (this.isSelectedLecture(lecture)) {
      this.removeSelectedLecture(lecture.courseCode, lecture.session)
    } else {
      this.addSelectedLecture(lecture)
      this.unhoverLecture()
    }
  }

  // Check whether the lecture is in the selectedLectures list, return true if it is, false it is not.
  isSelectedLecture(lecture) {
    const sameLecture = this.state.selectedLectures.filter(selectedLecture => {
      return (
        selectedLecture.courseCode === lecture.courseCode &&
        selectedLecture.session === lecture.session &&
        selectedLecture.lectureCode === lecture.lectureCode
      )
    })
    // If sameLecture is not an empty array, then this lecture is already selected and should be removed
    return sameLecture.length > 0
  }

  // Method passed to child component CoursePanel to hover over a lecture section
  hoverLecture(lecture) {
    if (!this.isSelectedLecture(lecture)) {
      this.setState({ hoveredLecture: lecture })
    }
  }

  // Method passed to child component CoursePanel to stop hovering over a lecture section
  unhoverLecture() {
    this.setState({ hoveredLecture: null })
  }

  setExportRef(r) {
    this.exportModal = r
  }

  render() {
    const updatedList = this.state.hoveredLecture
      ? this.state.selectedLectures.concat(this.state.hoveredLecture)
      : this.state.selectedLectures
    return (
      <div>
        <Disclaimer />
        <CoursePanel
          selectedCourses={this.state.selectedCourses}
          selectedLectures={this.state.selectedLectures}
          removeCourse={this.removeSelectedCourse}
          clearCourses={this.clearSelectedCourses}
          hoverLecture={this.hoverLecture}
          unhoverLecture={this.unhoverLecture}
          selectLecture={this.selectLecture}
          selectCourse={this.addSelectedCourse}
        />
        <Row lectureSections={updatedList} />
        <ExportModal context="grid" session="fall" ref={this.setExportRef} />
      </div>
    )
  }
}

const container = document.getElementById("grid-body")
const root = createRoot(container)
root.render(<Grid />)
