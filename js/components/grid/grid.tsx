import { useEffect, useState, useRef } from "react"

import { CoursePanel } from "./course_panel.js.jsx"
import { Row } from "./calendar.js.jsx"
import { ExportModal, ExportModalHandle } from "../common/export"
import Disclaimer from "../common/Disclaimer"
import { SelectedLecture } from "./types"

import {
  AllCommunityModule,
  ModuleRegistry,
  provideGlobalGridOptions,
} from "ag-grid-community"
import { NavBar } from "../common/NavBar.js.jsx"

// Register all community features
ModuleRegistry.registerModules([AllCommunityModule])

// Mark all grids as using legacy themes
provideGlobalGridOptions({ theme: "legacy" })

/**
 * Renders the course panel, the Fall and Spring timetable grids and search panel.
 * Also keeps track of all the selected courses and lectures.
 */
export default function Grid() {
  const [selectedLectures, setSelectedLectures] = useState<SelectedLecture[]>([])
  const [selectedCourses, setSelectedCourses] = useState<string[]>([])
  const [hoveredLecture, setHoveredLecture] = useState<SelectedLecture | null>(null)
  const [modalOpen, setModalOpen] = useState(false)

  const exportModal = useRef<ExportModalHandle>(null)

  // get the previously selected courses and lecture sections from local storage
  useEffect(() => {
    const selectedCoursesLocalStorage = localStorage.getItem("selectedCourses")
    const selectedLecturesLocalStorage = localStorage.getItem("selectedLectures")

    if (selectedLecturesLocalStorage) {
      try {
        setSelectedLectures(JSON.parse(selectedLecturesLocalStorage))
      } catch (e) {
        console.log(e)
      }
    }

    if (selectedCoursesLocalStorage) {
      const selectedCourses: string[] = []
      selectedCoursesLocalStorage.split("_").forEach(courseCode => {
        // Not using addSelectedCourse(courseCode) because each time addSelectedCourse is
        // called, setSelectedCourse is called.
        // setSelectedCourses is asynchronous and calling it several times in a row can lead to bugs when
        // new state depends on previous state
        selectedCourses.push(courseCode)
      })
      setSelectedCourses(selectedCourses)
    }
  }, [])

  useEffect(() => {
    localStorage.setItem("selectedCourses", selectedCourses.join("_"))
  }, [selectedCourses])

  useEffect(() => {
    localStorage.setItem("selectedLectures", JSON.stringify(selectedLectures))
  }, [selectedLectures])

  // Method passed to child component SearchPanel to add a course to selectedCourses.
  const addSelectedCourse = (courseCode: string) => {
    // updatedCourses is a copy of selectedCourses so that prevState can be distinguished from
    // the current state when the component updats during its lifecycle.
    const updatedCourses = selectedCourses.slice()
    updatedCourses.push(courseCode)
    setSelectedCourses(updatedCourses)
  }

  // Method passed to child components, SearchPanel and CoursePanel to remove a course from selectedCourses.
  const removeSelectedCourse = (courseCode: string) => {
    const updatedCourses = selectedCourses.slice()
    const index = updatedCourses.indexOf(courseCode)
    updatedCourses.splice(index, 1)
    setSelectedCourses(updatedCourses)

    const updatedLectures = selectedLectures.filter(
      lecture => !lecture.courseCode.includes(courseCode)
    )
    setSelectedLectures(updatedLectures)
  }

  // Method passed to child component CoursePanel to clear all the courses in selectedCourses.
  const clearSelectedCourses = () => {
    setSelectedCourses([])
    setSelectedLectures([])
  }

  // Method passed to child component CoursePanel to add a lecture to selectedLectures
  const addSelectedLecture = (newLecture: SelectedLecture) => {
    // The maximum number of courses in the lecture list with the same code is 3, one for each session (F, S, Y)
    const updatedLectures = selectedLectures.filter(lecture => {
      return (
        lecture.courseCode !== newLecture.courseCode ||
        lecture.session !== newLecture.session
      )
    })
    updatedLectures.push(newLecture)
    setSelectedLectures(updatedLectures)
  }

  // Method passed to child component CoursePanel to remove a lecture from selectedLectures
  const removeSelectedLecture = (courseCode: string, session: string) => {
    const updatedLectures = selectedLectures.filter(lecture => {
      return lecture.courseCode !== courseCode || lecture.session !== session
    })
    setSelectedLectures(updatedLectures)
  }

  // Remove the lecture if it is already in the selectedLectures list, or add the lecture if it is not.
  const selectLecture = (lecture: SelectedLecture) => {
    if (isSelectedLecture(lecture)) {
      removeSelectedLecture(lecture.courseCode, lecture.session)
    } else {
      addSelectedLecture(lecture)
      unhoverLecture()
    }
  }

  // Check whether the lecture is in the selectedLectures list, return true if it is, false it is not.
  const isSelectedLecture = (lecture: SelectedLecture) => {
    const sameLecture = selectedLectures.filter(selectedLecture => {
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
  const hoverLecture = (lecture: SelectedLecture) => {
    if (!isSelectedLecture(lecture)) {
      setHoveredLecture(lecture)
    }
  }

  // Method passed to child component CoursePanel to stop hovering over a lecture section
  const unhoverLecture = () => {
    setHoveredLecture(null)
  }

  // Method passed to child component NavBar to open the export modal when the button is clicked
  const openExportModal = () => {
    setModalOpen(true)
  }

  // Method passed to child component ExportModal to close the export modal when esc is clicked,
  // or a click outside the modal is detected
  const closeExportModal = () => {
    setModalOpen(false)
  }

  const updatedList = hoveredLecture
    ? selectedLectures.concat(hoveredLecture)
    : selectedLectures
  return (
    <>
      <NavBar
        selected_page="grid"
        open_modal={openExportModal}
        updateGraph={undefined}
      ></NavBar>
      <ExportModal
        page="grid"
        open={modalOpen}
        onRequestClose={closeExportModal}
      ></ExportModal>
      <Disclaimer />
      <CoursePanel
        selectedCourses={selectedCourses}
        selectedLectures={selectedLectures}
        removeCourse={removeSelectedCourse}
        clearCourses={clearSelectedCourses}
        hoverLecture={hoverLecture}
        unhoverLecture={unhoverLecture}
        selectLecture={selectLecture}
        selectCourse={addSelectedCourse}
      />
      <Row lectureSections={updatedList} />
      <ExportModal context="grid" session="fall" ref={exportModal} />
    </>
  )
}
