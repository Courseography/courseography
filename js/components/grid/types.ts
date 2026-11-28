import type { LectureTimeframe, TimeData } from "../common/types"

/** 
 * A lecture section a user can select, which persists to localStorage. 
 * 
 * `lecturesOnly` is true if this section's course only has LEC sections.    
 * `dataSatisfied` is true if all required section types (LEC and TUT/PRA if applicable) are selected.
 */
export interface SelectedLecture {
  courseCode: string
  lectureCode: string
  session: string
  times: TimeData[]
  lecturesOnly: boolean
  dataSatisfied?: boolean
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
