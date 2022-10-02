/** Helper function for parsing hybrid node's text.
 * 
 * @param {string} s a combination of course codes
 * @returns {Array} an array containing parsed course codes
 */
export function parseAnd(s) {
  "use strict"
  console.log(s)
  var curr = s
  var andList = []
  while (curr.length > 0) {
    if (curr.charAt(0) === "," || curr.charAt(0) === ";" || curr.charAt(0) === " ") {
      curr = curr.substr(1)
    } else {
      var result = parseOr(curr)
      console.log(result)
      if (curr === result[1]) {
        console.error("Parsing failed for " + s + "  with curr = " + curr)
        break
      } else {
        curr = result[1]
        andList.push(result[0])
      }
    }
  }
  return [andList, curr]
}

/**
 * Helper function for parsing hybrid node's text.
 * Calls parseCourse to parse courses separated by '/'.
 * If the resulting parsed list only has one course, return the course
 * itself as the first element as the return array.
 * If there is a course separated by ',', stop and return the remaining
 * courses as the second element in the return array.
 * 
 * @param {string} s a combination of course codes
 * @returns {Array} an array containing parsed course codes
 */
export function parseOr(s) {
  "use strict"
  var curr = s
  var orList = []
  var tmp
  var result
  var coursePrefix
  while (curr.length > 0 && curr.charAt(0) !== "," && curr.charAt(0) !== ";") {
    if (curr.charAt(0) === "(") {
      tmp = curr.substr(1, curr.indexOf(")"))
      if (coursePrefix === undefined && tmp.length >= 6) {
        coursePrefix = tmp.substr(0, 3).toUpperCase()
      }
      
      result = parseCourse(tmp, coursePrefix)
      orList.push(result[0])
      curr = curr.substr(curr.indexOf(")") + 1)
    } else if (curr.charAt(0) === " " || curr.charAt(0) === "/") {
      curr = curr.substr(1)
    } else {
      
      if (coursePrefix === undefined && curr.length >= 6) {
        coursePrefix = curr.substr(0, 3).toUpperCase()
      }
      result = parseCourse(curr, coursePrefix)
      if (curr === result[1]) {
        console.error("Parsing failed for " + s + " with curr = " + curr)
        break
      }
      curr = result[1]
      orList.push(result[0])
    }
  }
  // If only one course was parsed, return that course.
  if (orList.length === 1) {
    orList = orList[0]
  }

  return [orList, curr]
}

/**
 * Helper function for parsing hybrid node's text
 *
 * @param {string} s a combination of courses
 * @param {string} prefix prefix of the current course
 * @returns {Array} an array containing parsed courses
 */
export function parseCourse(s, prefix) {
  "use strict"

  var start = s.search(/[,/]/)
  if (start === 3) {
    return [prefix + s.substr(0, start), s.substr(start)]
  } else if (start > 0) {
    return [s.substr(0, start).toUpperCase(), s.substr(start)]
  }

  if (s.length === 3) {
    return [prefix + s, ""]
  }
  return [s, ""]
}
