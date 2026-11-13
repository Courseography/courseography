import { LectureTimeframe } from "../common/types"
import { SelectedLecture, TimetableMap } from "./types"

/**
 * Helper function to initialize the <fallSession> and <springSession> 2-D dictionaries
 * @param {list} lectures : List of 'Lecture' objects
 */
export const initializeSessions = (lectures: LectureTimeframe[]): TimetableMap[] => {
  const fallSession = {} as TimetableMap
  const springSession = {} as TimetableMap
  for (let i = 7; i < 22; i += 0.5) {
    fallSession[i] = { 0: [], 1: [], 2: [], 3: [], 4: [] }
    springSession[i] = { 0: [], 1: [], 2: [], 3: [], 4: [] }
  }

  lectures.forEach(lecture => {
    if (lecture.session === "F" || lecture.session === "Y") {
      for (let i = lecture.startTime; i < lecture.endTime; i += 0.5) {
        // Store this Lecture in its active time-slot (denoted by <i>),
        // and in the list in its active day slot (denoted by <lecture.day>), in <fallSession>.
        fallSession[i][lecture.day].push(lecture)
      }
    }
    if (lecture.session === "S" || lecture.session === "Y") {
      // Same process as above for spring lectures in <springSession>
      for (let i = lecture.startTime; i < lecture.endTime; i += 0.5) {
        springSession[i][lecture.day].push(lecture)
      }
    }
  })
  return [fallSession, springSession]
}

/**
 * Helper function which sets the width of each Lecture,
 * and also sets the inConflict attribute of all Lecture objects
 * @param {dictionary} session : 2-D Dictionary storing lectures active in the session
 */
export function setWidths(session: TimetableMap) {
  for (let i = 8; i < 22; i += 0.5) {
    const timeRow = session[i]
    // Reset all widths back to 1 and inConflict to false if this time slot is the startTime of
    // the lecture
    for (let day = 0; day < 5; day++) {
      const timeDaySlot = timeRow[day]
      timeDaySlot.forEach(lecture => {
        // Do not set the width back to 1 if there is already a conflict (this situation applies to
        // 'Y' session lectures)
        if (lecture.startTime === i && lecture.width === 1) {
          lecture.width = 1
          lecture.inConflict = false
        }
        // If in this time-and-day slot there is a lecture conflict
        if (lectureConflict(timeDaySlot)) {
          // Readjust (if needed) the 'width's of the lectures at this slot
          if (lecture.width < timeDaySlot.length) {
            lecture.width = timeDaySlot.length
          }
          // Also specify that this lecture is in conflict
          lecture.inConflict = true
        }
      })
    }
  }
}

/**
 * Helper function which stores the 'colSpan' attribute value for each day in the table header
 * @param {dictionary} colSpans : Dictionary storing the 'colSpan' attribute value for each day
 */
export const storeColSpans = (session: TimetableMap, colSpans: Record<number, number>): void => {
  // For each day, stores all possible 'width' values every lecture at that day could have
  const widths = storeWidths(session)
  for (let day = 0; day < 5; day++) {
    // The 'colSpan' attribute of a header day cell is defined as the product of all
    // possible widths that could occur at that day
    colSpans[day] = widths[day].reduce((a, b) => a * b, 1)
  }
}

/**
 * Helper function which, for each day, stores all possible 'width' values the lectures in that day could have
 * @param {dictionary} session : 2-D Dictionary storing lectures active in the session
 */
const storeWidths = (session: TimetableMap): Record<number, number[]> => {
  const widths = {} as Record<number, number[]>
  // Iterate through every time-day slot in the session
  for (let day = 0; day < 5; day++) {
    widths[day] = []
    for (let i = 8; i < 22; i += 0.5) {
      const timeRow = session[i]
      const timeDaySlot = timeRow[day]
      // If there are lectures running at this time-day slot
      if (timeDaySlot.length > 0) {
        // Retrieve a lecture width at this time-day slot
        const width = timeDaySlot[0].width
        // Store the width of this lecture, if it hasn't already been stored
        if (widths[day].indexOf(width) < 0) {
          widths[day].push(width)
        }
      }
    }
  }
  return widths
}

/**
 * Helper function which returns if there is a course conflict at a time-and-day slot
 * (denoted by the number of courses in the list at this time-and-day slot)
 */
const lectureConflict = (courseList: LectureTimeframe[]): boolean => {
  return courseList.length > 1
}

/**
 * Create a list of lecture objects representing each separate time a given lecture section takes place
 * @param lectureSection : Represents a lecture section for a course with time periods in which the lecture
 *                            takes place
 * @return {LectureTimeframe[]} An array of objects representing each disconnected time period the given
 *                  lecture occurs in
 */
export const createNewLectures = (lectureSection: SelectedLecture, selectedLectures: SelectedLecture[]): LectureTimeframe[] => {
  // dataSatisfied is false if a tutorial or practical for a course is selected in a particular session(F or S) and there
  // is no lecture selected for that course in the given session.
  lectureSection.dataSatisfied = false
  const lectureSelected = selectedLectures.filter(lecture => {
    return (
      lecture.lectureCode.substring(0, 1) === "L" &&
      lecture.courseCode.substring(0, 6) ===
        lectureSection.courseCode.substring(0, 6) &&
      lecture.session === lectureSection.session
    )
  })
  if (lectureSelected.length > 0) {
    lectureSection.dataSatisfied = true
  }

  const dayStrings = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
  const lectures = lectureSection.times.map(time => {
    const timeframe: LectureTimeframe = {
      courseCode: lectureSection.courseCode,
      session: lectureSection.session,
      timeSession: time.timeSession,
      day: time.weekDay,
      dayString: dayStrings[time.weekDay],
      startTime: time.startHour!,
      endTime: time.endHour!,
      location: time.timeLocation,
      inConflict: false,
      width: 1,
      dataSatisfied: lectureSection.dataSatisfied,
    }
    return timeframe
  })
  return lectures
}
