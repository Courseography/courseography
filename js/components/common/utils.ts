import type { ProgramData, ProgramNotModified } from "./types"
/**
 * Retrieves a course from file.
 * @param courseName The course code. This + '.txt' is the name of the file.
 * @returns Promise object representing the JSON object containing course information
 *                    or null if not found.
 */
export function getCourse(courseName: string): Promise<unknown> {
  "use strict"

  return fetch("course?name=" + courseName).then(response => {
    if (!response.ok) {
      throw new Error(`Failed to fetch course with name ${courseName}`)
    }

    return response.json()
  })
}

/**
 * Retrieves a program from the server.
 * @param programCode The program code on the art&sci timetable.
 * @param lastModified The last time the client called this function in UTC time
 * @returns Promise object representing the JSON object containing program information and
 *                    a boolean of whether the data was modified since last time
 */
export function getProgram(
  programCode: string,
  lastModified: string
): Promise<ProgramData | ProgramNotModified> {
  "use strict"

  return fetch("post?code=" + programCode, {
    headers: {
      "If-Modified-Since": lastModified,
    },
  })
    .then(async (response): Promise<ProgramData | ProgramNotModified> => {
      if (response.status === 304) return { modified: false }

      const responseJson = await response.json()

      return {
        title: responseJson.programDepartment,
        description: responseJson.programDescription,
        requirements: responseJson.programRequirements,
        courseList: getCourseList(responseJson.programRequirements),
        modified: true,
        modifiedTime: response.headers.get("Last-modified"),
      }
    })
    .catch(error => {
      throw error
    })
}

/**
 * Parses the course codes from a requirement text and converts them to lower case
 * @param requirements The requirement text containing the course codes
 * @returns Promise object representing an array of required or related courses.
 */
function getCourseList(requirements: string): string[] {
  const courseCodeRegex = /[A-Z]{3}[0-9]{3}(?=[HY][135])/g
  const courseList = requirements.match(courseCodeRegex) || []
  return courseList.map(course => course.toLowerCase())
}

// Helper function to get the string representation of the time
export function convertTimeToString(time: number): string {
  const meridiem = time < 12 ? "AM" : "PM"

  if (time % 1 === 0) {
    return (time === 12 ? 12 : time % 12) + ":00" + meridiem
  }
  return ((time === 12.5 ? 12 : Math.floor(time)) % 12) + ":30" + meridiem
}
