import type { JSX } from "react"

interface Building {
  buildingCode: string
  buildingName?: string
  buildingAddress?: string
  buildingPostalCode?: string
  buildingLat?: number
  buildingLng?: number
}

export interface TimeData {
  weekDay: 0 | 1 | 2 | 3 | 4
  startHour?: number
  endHour?: number
  timeLocation?: Building | null
  timeSession?: string
}

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

export interface Sessions {
  F?: SessionRowData[]
  S?: SessionRowData[]
  Y?: SessionRowData[]
  [key: string]: SessionRowData[] | undefined
}

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

export interface TimeframeData {
  courseCode: string
  session: string
  startTime: number
  endTime: number
  room?: string
}

export interface DayData {
  dayString: string
  dayOrder: number
  timeframes: TimeframeData[]
}

export interface BuildingLocation {
  buildingCode?: string
  buildingName?: string
  buildingAddress?: string
  buildingPostalCode?: string
  buildingLat?: number
  buildingLng?: number
  room?: string
}

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

export interface BuildingGroup {
  buildingName?: string
  buildingCode?: string
  address?: string
  lat?: number
  lng?: number
  postalCode?: string
  days: DayData[]
}

export interface ProgramData {
  modified: true
  title: string
  description: string
  requirements: string
  courseList: string[]
  modifiedTime: string | null
}

export interface ProgramNotModified {
  modified: false
}

export type Session = "F" | "S" | "Y"