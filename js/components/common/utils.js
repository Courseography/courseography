/**
 * Retrieves a course from file.
 * @param {string} courseName The course code. This + '.txt' is the name of the file.
 * @returns {Promise} Promise object representing the JSON object containing course information.
 */
export function getCourse(courseName) {
  "use strict"

  // look for courseName in database to whether add H1 or Y1 to the end
  return fetch("course?name=" + courseName + "H1")
    .then(response => response.json())
    .catch(error => {
      throw error
    })
}

/**
 * Retrieves a post from the server.
 * @param {string} postCode The post code on the art&sci timetable.
 * @param {string} lastModified The last time the client called this function in UTC time
 * @returns {Promise} Promise object representing the JSON object containing post information and
 *                    a boolean of whether the data was modified since last time
 */
export function getPost(postCode, lastModified) {
  "use strict"

  return fetch("post?code=" + postCode, {
    headers: {
      "If-Modified-Since": lastModified,
    },
  })
    .then(async response => {
      if (response.status === 304) return { modified: false }

      const responseJson = await response.json()

      return {
        title: responseJson.postDepartment,
        description: responseJson.postDescription,
        requirements: responseJson.postRequirements,
        courseList: getCourseList(responseJson.postRequirements),
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
