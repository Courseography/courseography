import { type JSX, useEffect, useState } from "react"
import ReactModal from "react-modal"
import { Description } from "./Description"
import { getCourse } from "./utils"
import type {
  CourseDetails,
  MeetingItem,
  RawCourseData,
  Session,
  Sessions,
  SessionRowData,
  TimeData,
} from "./types"

const INT_TO_DAY: Record<number, string> = {
  0: "Monday",
  1: "Tuesday",
  2: "Wednesday",
  3: "Thursday",
  4: "Friday",
}

const COURSE_CODE_REGEX = /([A-Z]{3}[0-9]{3}[H|Y]1)/

interface CourseModalProps {
  showCourseModal?: boolean
  courseId?: string
  onClose?: () => void
}

/**
 * Converts course codes within a text string to interactive <a> tags.
 * @param content Text containing course codes to parse into links.
 * @param onNavigate Optional callback invoked when a course link is clicked.
 * @returns An array of strings and clickable JSX link elements.
 */
export const convertToLink = (
  content?: string | null,
  onNavigate?: (courseId: string) => void
): (string | JSX.Element)[] => {
  if (!content) {
    return []
  }

  const result: (string | JSX.Element)[] = content.split(COURSE_CODE_REGEX)
  for (let i = 0; i < result.length; i++) {
    const word = result[i]
    if (typeof word === "string" && word.match(COURSE_CODE_REGEX)) {
      result[i] = (
        <a key={i} className="course-selection" onClick={() => onNavigate?.(word)}>
          {word}
        </a>
      )
    }
  }
  return result
}

/**
 * Helper function to format a TimeData object for display in the course modal.
 * @param {TimeData} time A TimeData object.
 * @returns {string} The time formatted as a string (e.g. "Tuesday 11 - 13").
 */
const formatTime = (time: TimeData): string => {
  return INT_TO_DAY[time.weekDay] + " " + time.startHour + " - " + time.endHour
}

/**
 * Generate the data needed for the course modal table based on the meeting times corresponding
 * to a course in a given session.
 * @param {MeetingItem[]} allMeetTimes An array of MeetingItem objects corresponding to a particular course.
 * @param {Session} session The session (F, S, Y) to query.
 * @returns {SessionRowData[]} An array of row data objects that will appear in the course modal table.
 */
export const getTable = (
  allMeetTimes: MeetingItem[],
  session: Session
): SessionRowData[] => {
  // Filter and sort the lecture sections in the specified session by their section code
  const filteredMeetTimes = allMeetTimes.filter(
    meetTime => meetTime.meetData.session === session
  )

  const sortedMeetTimes = filteredMeetTimes.sort((firstMeetTime, secondMeetTime) =>
    firstMeetTime.meetData.section > secondMeetTime.meetData.section ? 1 : -1
  )

  return sortedMeetTimes.map(meetTime => {
    // Sort each section's meeting time blocks and corresponding locations and store them in arrays.
    // times2, locations2 are used for the extra columns that appear in the table when viewing a Y course.
    const occurrences = {
      times1: [] as string[],
      locations1: [] as string[],
      times2: [] as string[],
      locations2: [] as string[],
    }
    const sortedTimeData = meetTime.timeData.sort((time1, time2) =>
      time1.weekDay > time2.weekDay ? 1 : -1
    )
    sortedTimeData.map(time => {
      const location = time.timeLocation?.buildingCode ?? " "
      if (session === "Y" && time.timeSession?.endsWith("1")) {
        occurrences.locations2.push(location)
        occurrences.times2.push(formatTime(time))
      } else {
        occurrences.locations1.push(location)
        occurrences.times1.push(formatTime(time))
      }
    })
    const rowData = {
      activity: meetTime.meetData.section,
      instructor: meetTime.meetData.instructor,
      availability:
        meetTime.meetData.cap -
        meetTime.meetData.enrol +
        " of " +
        meetTime.meetData.cap +
        " available",
      waitList: meetTime.meetData.wait + " students",
      time1: occurrences.times1,
      location1: occurrences.locations1,
      time2: occurrences.times2,
      location2: occurrences.locations2,
    }

    return rowData
  })
}

/**
 * Modal popup that displays comprehensive information about a selected course.
 *
 * Fetches course data asynchronously and renders course descriptions, prerequisites,
 * exclusions, and a timetable grid for Fall, Spring, and Full Year sessions.
 * Manages an internal browser-like history stack allowing users to navigate
 * forward and backward between courses clicked within prerequisite text.
 *
 * @param showCourseModal Whether the modal popup is currently visible.
 * @param courseId The initial course code to display (e.g., "CSC110Y1").
 * @param onClose Callback invoked when the modal is requested to close.
 */
export function CourseModal({
  showCourseModal = false,
  courseId,
  onClose,
}: CourseModalProps): JSX.Element {
  const [course, setCourse] = useState<CourseDetails | null>(null)
  const [sessions, setSessions] = useState<Sessions>({})
  const [courseTitle, setCourseTitle] = useState<string>("")

  const [visitedCourses, setVisitedCourses] = useState<string[]>(
    courseId ? [courseId] : []
  )
  const [currVisitedIndex, setCurrVisitedIndex] = useState<number>(0)
  const currentCourseId = visitedCourses[currVisitedIndex] ?? ""

  /**
   * Updates state when a new course link is clicked.
   * Truncates any forward history and appends the new course code.
   * @param newCourseId The course code to navigate to.
   */
  const navigateToCourse = (newCourseId: string): void => {
    if (newCourseId === currentCourseId) return
    // Truncate any forward history and append the new course
    setVisitedCourses(prev => [...prev.slice(0, currVisitedIndex + 1), newCourseId])
    setCurrVisitedIndex(prev => prev + 1)
  }

  /**
   * Manages state for currVisitedIndex when we click the back button to
   * return to the previous course that we visited.
   */
  const navigateBack = (): void => {
    if (currVisitedIndex > 0) {
      setCurrVisitedIndex(prev => prev - 1)
    }
  }

  /**
   * Manages state for currVisitedIndex when we click the forward button to
   * advance to the next course that we visited.
   */
  const navigateForward = (): void => {
    if (currVisitedIndex < visitedCourses.length - 1) {
      setCurrVisitedIndex(prev => prev + 1)
    }
  }

  useEffect(() => {
    if (showCourseModal && courseId) {
      setVisitedCourses([courseId])
      setCurrVisitedIndex(0)
    }
  }, [showCourseModal, courseId])

  useEffect(() => {
    if (!currentCourseId) return
    getCourse(currentCourseId)
      .then(data => {
        const course = data as RawCourseData
        const newCourse = {
          ...course,
          description: convertToLink(course.description, navigateToCourse),
          prereqString: convertToLink(course.prereqString, navigateToCourse),
          coreqs: convertToLink(course.coreqs, navigateToCourse),
          prep: convertToLink(course.prep, navigateToCourse),
          exclusions: convertToLink(course.exclusions, navigateToCourse),
        }

        const sessions = {
          F: getTable(course.allMeetingTimes, "F"),
          S: getTable(course.allMeetingTimes, "S"),
          Y: getTable(course.allMeetingTimes, "Y"),
        }

        setCourse(newCourse)
        setSessions(sessions)
        setCourseTitle(`${course.name} ${course.title}`)
      })
      .catch(() => {
        console.error(`Course with code ${currentCourseId} not found`)
        setCourse(null)
        setSessions({})
        setCourseTitle("Course Not Found")
      })
  }, [currentCourseId])

  return (
    <ReactModal
      className="modal-class"
      overlayClassName="overlay"
      isOpen={showCourseModal}
      onRequestClose={onClose}
      ariaHideApp={false}
    >
      <div className="modal-header">
        {courseTitle}

        <div className="button-container">
          <button
            type="button"
            className="info-modal-button"
            onClick={navigateBack}
            disabled={currVisitedIndex === 0}
          >
            {"<"}
          </button>

          <button
            type="button"
            className="info-modal-button"
            onClick={navigateForward}
            disabled={currVisitedIndex === visitedCourses.length - 1}
          >
            {">"}
          </button>
        </div>
      </div>

      <div className="modal-body">
        {course && <Description course={course} sessions={sessions} />}
      </div>
    </ReactModal>
  )
}
