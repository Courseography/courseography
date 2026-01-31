/**
 * Retrieves a course from file.
 * @param {string} courseName The course code. This + '.txt' is the name of the file.
 * @returns {Promise} Promise object representing the JSON object containing course information
 *                    or null if not found.
 */
export function getCourse(courseName) {
  "use strict"

  return fetch("course?name=" + courseName)
    .then(response => {
      if (response.status === 404) {
        return null
      }

      if (!response.ok) {
        throw new Error(`Failed to fetch course with name ${courseName}`)
      }

      return response.json()
    })
    .catch(error => {
      throw error
    })
}

/**
 * Retrieves a program from the server.
 * @param {string} programCode The program code on the art&sci timetable.
 * @param {string} lastModified The last time the client called this function in UTC time
 * @returns {Promise} Promise object representing the JSON object containing program information and
 *                    a boolean of whether the data was modified since last time
 */
export function getProgram(programCode, lastModified) {
  "use strict"

  return fetch("post?code=" + programCode, {
    headers: {
      "If-Modified-Since": lastModified,
    },
  })
    .then(async response => {
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
 * @param {string} requirements The requirement text containing the course codes
 * @returns {Array<string>} Promise object representing an array of required or related courses.
 */
function getCourseList(requirements) {
  const courseCodeRegex = /[A-Z]{3}[0-9]{3}(?=[HY][135])/g
  const courseList = requirements.match(courseCodeRegex) || []
  return courseList.map(course => course.toLowerCase())
}
