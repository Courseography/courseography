/** Helper function for parsing hybrid node's text
 *
 * @param {string} s
 * @returns {Array}
 */
export function parseAnd(s) {
  "use strict"

  var curr = s
  var andList = []
  while (curr.length > 0) {
    if (curr.charAt(0) === "," || curr.charAt(0) === ";" || curr.charAt(0) 
=== " ") {
      curr = curr.substr(1)
    } else {
      var result = parseOr(curr)
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
 * @param {string} s
 * @returns {Array}
 */
export function parseOr(s) {
  "use strict"

  var curr = s
  var orList = []
  var tmp
  var result
  var coursePrefix
  while (curr.length > 0 && curr.charAt(0) !== "," && curr.charAt(0) !== 
";") {
    if (curr.charAt(0) === "(") {
      tmp = curr.substr(1, curr.indexOf(")"))
      if (coursePrefix === undefined && tmp.length >= 6) {
        coursePrefix = tmp.substr(0, 3).toUpperCase()
      }
      result = parseCourse(tmp, coursePrefix)

      orList.append(result[0])
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

  if (orList.length === 1) {
    orList = orList[0]
  }

  return [orList, curr]
}

/**
 * Helper function for parsing hybrid node's text
 * @param {string} s
 * @param {string} prefix
 * @returns {Array}
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

