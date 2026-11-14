import { LectureTimeframe, TimeData } from "../common/types"

/** A lecture section a user has selected, persisted to localStorage. */
export interface SelectedLecture {
  courseCode: string
  lectureCode: string
  session: string
  times: TimeData[]
  dataSatisfied: boolean
}

/**
 * A map representing a single row (half-hour interval) in a semester's timetable.
 * Maps each weekday to an array of timeframes contained in the timetable in this row's time interval.
 */
export type TimetableInterval = Record<number, LectureTimeframe[]>

/**
 * A map representing a semester's timetable.
 * Maps each half-hour interval's start time to a TimetableInterval containing its timeframes.
 * For example, any timeframes that contain the "13:30-14:00" interval will be mapped to "13.5".
 */
export type TimetableMap = Record<number, TimetableInterval>
