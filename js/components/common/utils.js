/**
 * Search for target node in list of nodes,
 * or if node not found search through list of bools.
 * @param {React.PropTypes.node} targetNode
 * @param {React.PropTypes.element} elem
 * @returns {React.PropTypes.Node}
 */
export function refLookUp(targetNode, svg) {
  return svg.nodes.current[targetNode] || svg.bools.current[targetNode]
}

/**
 * Retrieves a course from file.
 * @param {string} courseName The course code. This + '.txt' is the name of the file.
 * @returns {Promise} Promise object representing the JSON object containing course information.
 */
export function getCourse(courseName) {
  "use strict"

  return fetch("course?name=" + courseName)
    .then(response => response.json())
    .catch(error => {
      throw error
    })
}

/**
 * Retrieves a post from the server.
 * @param {string} postCode The post code on the art&sci timetable.
 * @returns {Promise} Promise object representing the JSON object containing post information.
 */
export function getPost(postCode) {
  "use strict"

  return fetch("post?code=" + postCode)
    .then(async response => {
      const responseJson = await response.json()
      const info = {
        description: responseJson.postDescription,
        requiredCourses: [],
        relatedCourses: [],
      }

      let field
      for (const line of responseJson.postRequirements.split("\n")) {
        if (line.toLowerCase().includes("required courses")) {
          field = info.requiredCourses
        } else if (line.toLowerCase().includes("related courses")) {
          field = info.relatedCourses
        } else if (field) {
          field.push(line)
        }
      }

      return {
        title: responseJson.postDepartment,
        info: info,
      }
    })
    .catch(error => {
      throw error
    })
}

/**
 * Retrieves a post from the server, then parses all course codes from the requirements.
 * @param {string} postCode The post code on the art&sci timetable.
 * @returns {Promise} Promise object representing an array of required or related courses.
 */
export function getPostCourseList(postCode) {
  "use strict"

  return fetch("post?code=" + postCode)
    .then(async response => {
      const responseJson = await response.json()
      const courseList =
        responseJson.postRequirements.match(/[A-Z]{3}[0-9]{3}(?=[HY][135])/g) || []
      return courseList.map(course => course.toLowerCase())
    })
    .catch(error => {
      throw error
    })
}
