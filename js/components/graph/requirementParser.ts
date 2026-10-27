/**
 * A nested list of courses as an AND of ORs (or an OR of ANDs), or the course itself if no
 * splitting is made.
 */
export type PrereqTree = string | PrereqTree[]

/**
 * Parse a logical prerequisite string as a conjunction of disjunctions.
 * @param s the prerequisite string
 * @returns a nested list of courses as an AND of ORs, or the course itself if no
 *  splitting is made
 */
export function parseAnd(s: string): PrereqTree {
  // Base case: return the course if no splitting is to be made.
  if (!s.includes(",") && !s.includes(";") && !s.includes("/")) {
    return removeOuterParens(s)
  }
  // Otherwise, recurse and parse each conjunctive as a disjunction.
  const andList = splitPrereqString(removeOuterParens(s.replaceAll(";", ",")), ",")
  let splitList: PrereqTree[] = []
  for (const str of andList) {
    if (str.length > 0) {
      splitList.push(parseOr(str))
    }
  }

  // Modify the returned list to account for shorthand course codes and remove grade requirements
  parseSplitList(splitList)
  return splitList
}

/**
 * Parse a logical prerequisite string as a disjunction of conjunctions.
 * @param s the prerequisite string
 * @returns a nested list of courses as an OR of ANDs, or the course itself if no
 *  splitting is made
 */
export function parseOr(s: string): PrereqTree {
  // Base case: return the course if no splitting is to be made.
  if (!s.includes(",") && !s.includes(";") && !s.includes("/")) {
    return removeOuterParens(s)
  }
  // Otherwise, recurse and parse each conjunctive as a disjunction.
  const orList = splitPrereqString(removeOuterParens(s), "/")
  let splitList: PrereqTree[] = []
  for (const str of orList) {
    if (str.length > 0) {
      splitList.push(parseAnd(str))
    }
  }

  // Modify the returned list to account for shorthand course codes and remove grade requirements
  parseSplitList(splitList)
  return splitList
}

/**
 * Helper function to split a prerequisite string by its 'and' or 'or' separator, and
 * strip the result of top-level outer parentheses and spaces.
 * @param s the prerequisite string
 * @param separator the separator to split by (',' for and, '/' for or)
 * @returns the resulting list of conjunctives/disjunctives
 */
export function splitPrereqString(s: string, separator: string): string[] {
  let splitList: string[] = []
  let currIndex = 0
  let curr = ""
  let parenLayer = 0 // Depth of nested parentheses
  while (currIndex < s.length) {
    // If a parenthesis is encountered, update parenLayer
    if (s.charAt(currIndex) === "(") {
      parenLayer += 1
    } else if (s.charAt(currIndex) === ")") {
      parenLayer -= 1
    }

    if (s.charAt(currIndex) === separator && parenLayer === 0) {
      // If the separator is encountered and we aren't inside parentheses, split on it and reset curr
      splitList.push(removeOuterParens(curr))
      curr = ""
    } else if (s.charAt(currIndex) !== " ") {
      // Add all other non-space characters to curr
      curr += s.charAt(currIndex)
    }
    currIndex += 1
  }
  splitList.push(removeOuterParens(curr))

  return splitList
}

/**
 * Helper function to strip a string entirely contained within a pair of parentheses.
 * @param s the prerequisite string to strip parentheses from
 * @returns the same string with all fully-enclosing pairs of parentheses removed
 */
export function removeOuterParens(s: string): string {
  if (s.length < 2 || s.charAt(0) !== "(" || s.charAt(s.length - 1) !== ")") {
    return s
  }

  let startIndex = 0
  while (s.charAt(startIndex) === "(" && s.charAt(s.length - 1 - startIndex) === ")") {
    startIndex += 1
  }
  let parenLayer = startIndex // Depth of nested parentheses
  let minParenLayer = startIndex // Minimum depth seen so far
  // Iterate through the string outside its opening '(' and closing ')' to find the minimum nest depth.
  for (let i = startIndex; i <= s.length - 1 - startIndex; i++) {
    if (s.charAt(i) === "(") {
      parenLayer += 1
    }
    if (s.charAt(i) === ")") {
      parenLayer -= 1
      if (parenLayer < minParenLayer) {
        minParenLayer = parenLayer
      }
    }
    if (parenLayer === 0) {
      // If we reach a nest depth of 0 prior to the end of the string, it isn't contained in parentheses.
      return s
    }
  }

  // Strip the amount of outer layers equal to the minimum nest depth.
  return s.substr(minParenLayer, s.length - minParenLayer * 2)
}

/**
 * Helper function to expand shorthand course codes (e.g. "MAT237/257") in-place from a list of course
 * strings, and remove any grade requirement strings (e.g. "MAT137 (73%)")
 * @param splitList the nested array to modify, representing a partially parsed prerequisite string
 */
export function parseSplitList(splitList: PrereqTree[]): void {
  let currPrefix = ""
  for (let i = 0; i < splitList.length; i++) {
    if (typeof splitList[i] === "object") {
      currPrefix = ""
    } else if (typeof splitList[i] === "string") {
      let course = splitList[i] as string

      // Filter out a grade requirement from the current course string
      let matchResult = course.match(/^(.+)\(.*%\)$/)
      if (matchResult !== null) {
        course = matchResult[1]
      }

      // Update currPrefix if the current course string contains a prefix
      if (course.match(/^[A-Z]{3}/g)) {
        currPrefix = course.substr(0, 3)
      }
      // Append currPrefix if the current course string is missing a prefix
      else if (course.match(/^[0-9]{3}$/g)) {
        course = currPrefix + course
      }

      splitList[i] = course
    }
  }
}
