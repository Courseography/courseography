import { useState, useEffect, useCallback } from "react"
import { getCourse } from "../common/utils"
import { SelectedLecture } from "./types"
import { MeetingItem, TimeData } from "../common/types"
import { SectionList } from "./section_list"

export interface CourseProps {
  selectedLectures: SelectedLecture[]
  courseCode: string
  removeCourse: (courseCode: string) => void
  hoverLecture: (lecture: SelectedLecture) => void
  unhoverLecture: () => void
  selectLecture: (lecture: SelectedLecture) => void
  displayInfo: (courseId: any) => void
}

interface ParsedLecture {
  courseCode: string
  lectureCode: string
  session: string
  times: TimeData[]
}

interface ParsedCourse {
  courseCode: string
  F: ParsedLecture[]
  S: ParsedLecture[]
  Y: ParsedLecture[]
}

/**
 * A selected course with a delete button, and info button, which displays information
 * about the course when clicked.
 * Also retrieves the course information. It holds list of F, S and Y lecture, tutorial
 * and practical sections for the course.
 */

export function Course({
  selectedLectures,
  courseCode,
  removeCourse,
  hoverLecture,
  unhoverLecture,
  selectLecture,
  displayInfo,
}: CourseProps) {
  const [selected, setSelected] = useState(false)
  const [courseInfo, setCourseInfo] = useState<ParsedCourse>({
    courseCode: "",
    F: [],
    S: [],
    Y: [],
  })

  const filterLectureList = useCallback(
    (lectures: ParsedLecture[], session: string): ParsedLecture[] => {
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

  const parseLectures = useCallback((lectures: MeetingItem[]): ParsedLecture[] => {
    const parsedLectures: ParsedLecture[] = []

    // Loop through the lecture sections to get each section's session code and lecture times
    lectures.forEach(lectureInfo => {
      // Check to make sure its not an online section (online sections have course codes beginning with 9) or
      // restricted section. Restricted sections have enrollment restricted for a particular group of students,
      // but happens at the same time and place as a regular lecture/tutorial section.
      if (
        lectureInfo.meetData.section.charAt(3) !== "2" &&
        lectureInfo.meetData.section.charAt(3) !== "9"
      ) {
        const lecture: ParsedLecture = {
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
      const lectures = selectedLectures.map(lecture =>
        lecture.courseCode.substring(0, 6)
      )
      const courseCode = courseInfo.courseCode
      return lectures.indexOf(courseCode.substring(0, 6)) >= 0
    }
    return false
  }, [selectedLectures])

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
        <h3
          onClick={toggleSelect}
          data-satisfied="true"
          data-taken={containsSelectedLecture() ? "true" : "false"}
        >
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
