import type { JSX } from "react"

/**
 * Literal union representing academic sessions:
 * - "F": Fall session (September to December)
 * - "S": Winter / Spring session (January to April)
 * - "Y": Full Year session (September to April)
 */
export type Session = "F" | "S" | "Y"

/**
 * Physical campus building information with coordinates and address details.
 */
interface Building {
  buildingCode: string
  buildingName?: string
  buildingAddress?: string
  buildingPostalCode?: string
  buildingLat?: number
  buildingLng?: number
}

/**
 * Scheduled meeting timeframe block for an individual course section,
 * representing day of the week, start/end hours, location, and session code.
 * Used in CourseModal and timetable generation.
 */
export interface TimeData {
  /** Day of the week: 0 = Monday, 1 = Tuesday, 2 = Wednesday, 3 = Thursday, 4 = Friday */
  weekDay: 0 | 1 | 2 | 3 | 4
  startHour?: number
  endHour?: number
  timeLocation?: Building | null
  timeSession?: string
}

/**
 * Instructional section (e.g., lecture, tutorial, practical) containing enrollment
 * figures, instructor name, session code, and associated meeting time blocks.
 */
export interface MeetingItem {
  meetData: {
    session: string
    section: string
    instructor: string
    cap: number
    enrol: number
    wait: number
  }
  timeData: TimeData[]
}

/**
 * Processed course details displayed inside CourseModal and Description.
 * Text fields like prerequisites, corequisites, and exclusions have been parsed
 * and converted into clickable course link elements where applicable.
 */
export interface CourseDetails {
  breadth?: string | null
  description?: (string | JSX.Element)[]
  title?: string | null
  prereqString?: (string | JSX.Element)[]
  allMeetingTimes: MeetingItem[]
  name?: string
  prep?: (string | JSX.Element)[]
  exclusions?: (string | JSX.Element)[]
  distribution?: string | null
  coreqs?: (string | JSX.Element)[]
}

/**
 * Row data model for the AG Grid table within Description.tsx,
 * displaying section activity, instructor, availability, waitlist,
 * and meeting times/locations for up to two time blocks (e.g. for Full Year courses).
 */
export interface SessionRowData {
  activity: string
  instructor: string
  availability: string
  waitList: string
  time1: string[]
  location1: string[]
  time2: string[]
  location2: string[]
}

/**
 * Timetable data partitioned by academic session ("F", "S", "Y")
 * containing arrays of table rows rendered in Description.tsx.
 */
export interface Sessions {
  F?: SessionRowData[]
  S?: SessionRowData[]
  Y?: SessionRowData[]
  [key: string]: SessionRowData[] | undefined
}

/**
 * Geographic coordinate and room details for a lecture location,
 * used to plot building markers in CampusMap.tsx.
 */
export interface BuildingLocation {
  buildingCode?: string
  buildingName?: string
  buildingAddress?: string
  buildingPostalCode?: string
  buildingLat?: number
  buildingLng?: number
  room?: string
}

/**
 * Scheduled lecture timeslot used in MapModal, MapSidebar, DayBox, and CampusMap.
 * Tracks course code, session, time range, geographic location, and conflict status.
 */
export interface LectureTimeframe {
  courseCode: string
  session: string
  timeSession?: string | null
  day: number
  dayString: string
  startTime: number
  endTime: number
  location?: BuildingLocation | null
  inConflict?: boolean
  width?: number
  dataSatisfied?: boolean
}

/**
 * Individual lecture occurrence within a day group, specifying time boundaries and room.
 */
export interface TimeframeData {
  courseCode: string
  session: string
  startTime: number
  endTime: number
  room?: string
}

/**
 * Collection of scheduled lecture timeframes occurring on a specific weekday.
 * Used for ordering and rendering daily schedules in MapSidebar and DayBox.
 */
export interface DayData {
  dayString: string
  dayOrder: number
  timeframes: TimeframeData[]
}

/**
 * Unprocessed course data retrieved directly from the backend endpoint
 * prior to parsing text descriptions and requirement links in CourseModal.tsx.
 */
export interface RawCourseData {
  breadth?: string | null
  description?: string | null
  title?: string | null
  prereqString?: string | null
  allMeetingTimes: MeetingItem[]
  name?: string
  prep?: string | null
  exclusions?: string | null
  distribution?: string | null
  coreqs?: string | null
  videoUrls?: string[]
}

/**
 * Aggregated lecture schedules grouped by physical campus building.
 * Passed to CampusMap to render map markers and calculate bounding boxes.
 */
export interface BuildingGroup {
  buildingName?: string
  buildingCode?: string
  address?: string
  lat?: number
  lng?: number
  postalCode?: string
  days: DayData[]
}

/**
 * Degree focus or program data retrieved from the backend when modified.
 * Used in FocusModal to display focus title, description, requirements, and courses.
 */
export interface ProgramData {
  modified: true
  title: string
  description: string
  requirements: string
  courseList: string[]
  modifiedTime: string | null
}

/**
 * Response received when program data has not changed since last request.
 */
export interface ProgramNotModified {
  modified: false
}
