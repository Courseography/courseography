import { type JSX, useState, useEffect, useCallback } from "react"
import { getCourse } from "../common/utils"
import type { SelectedLecture } from "./types"
import type { MeetingItem } from "../common/types"
import { SectionList } from "./section_list"

export interface CourseProps {
  courseCode: string
  removeCourse: (courseCode: string) => void
  hoverLecture: (lecture: SelectedLecture) => void
  unhoverLecture: () => void
  selectLecture: (lecture: SelectedLecture) => void
  displayInfo: (courseId: string) => void
}

interface ParsedCourse {
  courseCode: string
  F: SelectedLecture[]
  S: SelectedLecture[]
  Y: SelectedLecture[]
}

/**
 * A selected course with a delete button, and info button, which displays information
 * about the course when clicked.
 * Also retrieves the course information. It holds list of F, S and Y lecture, tutorial
 * and practical sections for the course.
 */
export function Course({
  courseCode,
  removeCourse,
  hoverLecture,
  unhoverLecture,
  selectLecture,
  displayInfo,
}: CourseProps): JSX.Element {
  const [selected, setSelected] = useState(false)
  const [courseInfo, setCourseInfo] = useState<ParsedCourse>({
    courseCode: "",
    F: [],
    S: [],
    Y: [],
  })

  const filterLectureList = useCallback(
    (lectures: SelectedLecture[], session: string): SelectedLecture[] => {
      return lectures
        .filter(lec => lec.session === session)
        .sort((firstLec, secondLec) =>
          firstLec.lectureCode > secondLec.lectureCode ? 1 : -1
        )
    },
    []
  )

  const toggleSelect = useCallback(() => {
    setSelected(s => !s)
  }, [])

  const removeCourseCallback = useCallback(() => {
    removeCourse(courseCode)
  }, [removeCourse, courseCode])

  const parseLectures = useCallback((lectures: MeetingItem[]): SelectedLecture[] => {
    const parsedLectures: SelectedLecture[] = []
    const lecturesOnly = !lectures.some(lectureInfo => {
      return (
        lectureInfo.meetData.section.substring(0, 1) === "T" || 
        lectureInfo.meetData.section.substring(0, 1) === "P"
      )
    })

    // Loop through the lecture sections to get each section's session code and lecture times
    lectures.forEach(lectureInfo => {
      // Check to make sure its not an online section (online sections have course codes beginning with 9) or
      // restricted section. Restricted sections have enrollment restricted for a particular group of students,
      // but happens at the same time and place as a regular lecture/tutorial section.
      if (
        lectureInfo.meetData.section.charAt(3) !== "2" &&
        lectureInfo.meetData.section.charAt(3) !== "9"
      ) {
        const lecture: SelectedLecture = {
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
          lecturesOnly: lecturesOnly
        }
        parsedLectures.push(lecture)
      }
    })
    return parsedLectures
  }, [])

  useEffect(() => {
    getCourse(courseCode)
      .then(data => {
        const course: ParsedCourse = {
          courseCode: "",
          F: [],
          S: [],
          Y: [],
        }

        course.courseCode = data.name ? data.name : ""

        const parsedLectures = parseLectures(data.allMeetingTimes)
        // Split the lecture sections into Fall, Spring and Years
        course.F = filterLectureList(parsedLectures, "F")
        course.S = filterLectureList(parsedLectures, "S")
        course.Y = filterLectureList(parsedLectures, "Y")
        setCourseInfo(course)
      })
      .catch(_ => {
        console.error(`Course with code ${courseCode} not found`)
        setCourseInfo({
          courseCode: "",
          F: [],
          S: [],
          Y: [],
        })
      })
  }, [])

  return (
    <li
      key={courseCode}
      id={courseCode + "-li"}
      className={"ui-accordion ui-widget ui-helper-reset"}
    >
      <div
        className="ui-accordion-header ui-helper-reset ui-state-default ui-accordion-icons ui-accordion-header-active ui-state-active ui-corner-top"
        id={"ui-accordion-" + courseCode + "-li-header-0"}
      >
        <div className="icon-div">
          <img
            src="/static/res/ico/delete.png"
            className="close-icon"
            onClick={removeCourseCallback}
          />
          <img
            src="/static/res/ico/about.png"
            className="close-icon"
            onClick={() => displayInfo(courseCode)}
          />
        </div>
        <h3 onClick={toggleSelect} data-satisfied="true">
          {courseCode}
        </h3>
      </div>
      {selected && (
        <div
          className="sections ui-accordion-content ui-helper-reset ui-widget-content ui-corner-bottom ui-accordion-content-active"
          id={"ui-accordion-" + courseCode + "-li-panel-0"}
        >
          <SectionList
            courseCode={courseCode}
            session="Y"
            lectures={courseInfo.Y}
            hoverLecture={hoverLecture}
            unhoverLecture={unhoverLecture}
            selectLecture={selectLecture}
          />
          <SectionList
            courseCode={courseCode}
            session="F"
            lectures={courseInfo.F}
            hoverLecture={hoverLecture}
            unhoverLecture={unhoverLecture}
            selectLecture={selectLecture}
          />
          <SectionList
            courseCode={courseCode}
            session="S"
            lectures={courseInfo.S}
            hoverLecture={hoverLecture}
            unhoverLecture={unhoverLecture}
            selectLecture={selectLecture}
          />
        </div>
      )}
    </li>
  )
}
