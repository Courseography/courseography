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
