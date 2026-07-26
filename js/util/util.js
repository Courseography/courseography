/**
 * Helper function to strip a string entirely contained within a pair of parentheses.
 */
export function removeOuterParens(s) {
  if (s.length < 2 || s.charAt(0) !== "(" || s.charAt(s.length - 1) !== ")") {
    return s
  }

  let parenLayer = 1 // Depth of nested parentheses
  // Iterate through the string outside its opening '(' and closing ')'.
  // If we reach a nest depth of 0 prior to the end of the string, it isn't contained in parentheses.
  for (let i = 1; i <= s.length - 2; i++) {
    if (s.charAt(i) === "(") {
      parenLayer += 1
    }
    if (s.charAt(i) === ")") {
      parenLayer -= 1
    }
    if (parenLayer === 0) {
      return s
    }
  }

  return s.substr(1, s.length - 2)
}

/**
 * Helper function to split a prerequisite string by its 'and' or 'or' separator.
 * Strip the result of top-level outer parentheses and spaces.
 * @param {string} s the prerequisite string
 * @param {string} separator the separator to split by (',' for and, '/' for or)
 * @returns the resulting list of conjunctives/disjunctives
 */
export function splitPrereqString(s, separator) {
  let splitList = []
  let currIndex = 0
  let curr = ""
  let parenLayer = 0 // Depth of nested parentheses
  while (currIndex < s.length) {
    // If a parenthesis is encountered, update parenLayer
    if (s.charAt(currIndex) === "(") {
      parenLayer += 1
    }
    if (s.charAt(currIndex) === ")") {
      parenLayer -= 1
    } 

    // If the separator is encountered and we aren't inside parentheses, split on it and reset curr
    if (s.charAt(currIndex) === separator && parenLayer === 0) {
      splitList.push(removeOuterParens(curr))
      curr = ""
    }
    // Add all other non-space characters to curr
    else if (s.charAt(currIndex) !== " ") {
      curr += s.charAt(currIndex)
    }
    currIndex += 1
  }
  splitList.push(removeOuterParens(curr))

  return splitList
}

/**
 * Parse a logical prerequisite string as a conjunction of disjunctions.
 * @param {string} s the prerequisite string
 * @returns a nested list of courses as an AND of ORs, or the course itself if no splitting is made
 */
export function parseAnd(s) {
  // Base case: return the course if no splitting is to be made.
  if (!s.includes(",") && !s.includes("/")) {
    return removeOuterParens(s)
  }
  // Otherwise, recurse and parse each conjunctive as a disjunction.
  const andList = splitPrereqString(removeOuterParens(s), ",")
  let splitList = []
  for (const str of andList) {
    splitList.push(parseOr(str))
  }
  // Expand any shorthand groups of strings (e.g. "MAT237,257") to their full course codes
  let currPrefix = ""
  for (let i = 0; i < splitList.length; i++) {
    if (typeof splitList[i] === "object") {
      currPrefix = ""
    } 
    else if (typeof splitList[i] === "string") {
      if (splitList[i].match(/^[A-Z]{3}/g)) {
        currPrefix = splitList[i].substr(0, 3)
      } else if (splitList[i].match(/^[0-9]{3}$/g)) {
        splitList[i] = currPrefix + splitList[i]
      }
    }
  }
  return splitList
}

/**
 * Parse a logical prerequisite string as a disjunction of conjunctions.
 * @param {string} s the prerequisite string
 * @returns a nested list of courses as an OR of ANDs, or the course itself if no splitting is made
 */
export function parseOr(s) {
  // Base case: return the course if no splitting is to be made.
  if (!s.includes(",") && !s.includes("/")) {
    return removeOuterParens(s)
  }
  // Otherwise, recurse and parse each conjunctive as a disjunction.
  const orList = splitPrereqString(removeOuterParens(s), "/")
  let splitList = []
  for (const str of orList) {
    splitList.push(parseAnd(str))
  }
  // Expand any shorthand groups of strings (e.g. "MAT237/257") to their full course codes
  let currPrefix = ""
  for (let i = 0; i < splitList.length; i++) {
    if (typeof splitList[i] === "object") {
      currPrefix = ""
    } 
    else if (typeof splitList[i] === "string") {
      if (splitList[i].match(/^[A-Z]{3}/g)) {
        currPrefix = splitList[i].substr(0, 3)
      } else if (splitList[i].match(/^[0-9]{3}$/g)) {
        splitList[i] = currPrefix + splitList[i]
      }
    }
  }
  return splitList
}
