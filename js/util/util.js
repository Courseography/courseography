/** Helper function for parsing hybrid node's text.
 *  Calls parseOr to for parsing courses that are separated by /
 *  Example:
 *  const result = parseAnd('CSC108, CSC148, MAT135/136')
 *  result === [['CSC108', 'CSC148', ['MAT135', 'MAT136']], '']
 * @param {string} s a combination of course codes
 * @returns {Array} an array containing parsed course codes
 */
export function parseAnd(s) {
  // input: CSC301/317/318/384/417/419
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
      // result == ['CSC301', 'CSC317', 'CSC318', 'CSC384', 'CSC417', 'CSC419', '']
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
 * Helper function for parsing hybrid node's text
 * Calls parseCourse to parse courses separated by '/'
 * If the resulting parsed list only has one course, return the course
 * itself as the first element as the return array
 * If there is a course separated by ',', stop and return the remaining
 * courses as the second element in the return array
 * Example:
 * const result = parsOr('CSC108/148/165')
 * result === [['CSC108', 'CSC148', 'CSC165'], '']
 *
 * const result1 = parseOr('CSC108')
 * result === ['CSC108', '']
 *
 * const result2 = parseOr('CSC108/145, MAT137')
 * result2 === [['CSC108', 'CSC145'], ',MAT137']
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
      // tmp is the current course
      // if there is no coursePrefix, use the first three letters of the course
      // the results are parsed courses
      // "CSC301/317/318/384/417/419", "CSC" -> ["CSC301", "/317/318/384/417/419"]
      result = parseCourse(tmp, coursePrefix)
      // accumulate the parsed course in orList
      // Error - list has no append but push
      orList.push(result[0])
      curr = curr.substr(curr.indexOf(")") + 1)
    } else if (curr.charAt(0) === " " || curr.charAt(0) === "/") {
      curr = curr.substr(1)
    } else {
      // same as above
      if (coursePrefix === undefined && curr.length >= 6) {
        coursePrefix = curr.substr(0, 3).toUpperCase()
      }
      // result should be same as above
      result = parseCourse(curr, coursePrefix)
      // if nothing happened in parseCourse -- current courses == remaining courses
      // after parsing, we raise an error
      if (curr === result[1]) {
        console.error("Parsing failed for " + s + " with curr = " + curr)
        break
      }
      // update curr string
      curr = result[1]
      // same as above
      orList.push(result[0])
    }
  }
  // if list length is 1 only return the course code
  if (orList.length === 1) {
    orList = orList[0]
  }

  return [orList, curr]
}

/**
 * Helper function for parsing hybrid node's text
 * Example:
 * const result = 'CSC108/148/165'
 * result === ['CSC108', '/148/165']
 *
 * const result1 = 'CSC108, MAT135'
 * result === ['CSC108', ',MAT135']
 * @param {string} s a combination of courses
 * @param {string} prefix prefix of the current course
 * @returns {Array} an array containing parsed courses
 */
export function parseCourse(s, prefix) {
  "use strict"

  // searches for anything in the string that match , or /
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
