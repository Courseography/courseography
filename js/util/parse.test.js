// Tests for parsing algorithms from js/util/util.js
import { parseAnd, parseOr, parseCourse } from "./util"

const recursiveChecker = (actual, expected) => {
  if (typeof actual === "string") {
    expect(actual).toMatch(expected)
  } else if (Array.isArray(actual)) {
    actual.forEach((arr, i) => {
      recursiveChecker(arr, expected[i])
    })
  }
}

describe("parseAnd", () => {
  test("parseAnd correctly parses courses when a comma separates two couress", () => {
    const input = "CSC111, STA247"
    const actual = parseAnd(input)
    const expected = [["CSC111", "STA247"], ""]
    recursiveChecker(actual, expected)
  })
  test("parseAnd correctly parses courses separated by both comma and slash", () => {
    const input = "CSC111, MAT135/136/137"
    const actual = parseAnd(input)
    const expected = [["CSC111", ["MAT135", "MAT136", "MAT137"]], ""]
    recursiveChecker(actual, expected)
  })

  test("parseAnd correctly parses courses separated by both ; and slash", () => {
    const input = "CSC111/; MAT135/136/137"
    const actual = parseAnd(input)
    const expected = [["CSC111", ["MAT135", "MAT136", "MAT137"]], ""]
    recursiveChecker(actual, expected)
  })
  test("parseAnd correctly parses courses separated by multiple commas and slash and ;", () => {
    const input = "CSC111, STA247, Calc1/; MAT135/136, CSC145/CSC165/; CSC108/199"
    const actual = parseAnd(input)
    const expected = [
      [
        "CSC111",
        "STA247",
        "CALC1",
        ["MAT135", "MAT136"],
        ["CSC145", "CSC165"],
        ["CSC108", "CSC199"],
      ],
      "",
    ]
    recursiveChecker(actual, expected)
  })
  
})

describe("parseOr", () => {
  test("parseOr correctly calls returns all parsed courses separated by /", () => {
    const input = "CSC111/207/209/258"
    const actual = parseOr(input)
    const expected = [["CSC111", "CSC207", "CSC209", "CSC258"], ""]
    recursiveChecker(actual, expected)
  })
  test("parseOr correctly returns parsed course when a string of a single course is wrapped in paranthesis", () => {
    const input = "(CSC207) Calc1"
    const actual = parseOr(input)
    const expected = [["CSC207", "Calc1"], ""]
    recursiveChecker(actual, expected)
  })
  test("parseOr correctly returns parsed course when the string only contains one course", () => {
    const input = "CSC207"
    const actual = parseOr(input)
    const expected = ["CSC207", ""]
    recursiveChecker(actual, expected)
  })
  test("parseOr correctly returns parsed course when a comma separates two courses and breaks after parsing the first course", () => {
    const input = "CSC207,209"
    const actual = parseOr(input)
    const expected = ["CSC207", ",209"]
    recursiveChecker(actual, expected)
  })
  test("parseOr correctly returns parsed course when there is empty space between some of the courses", () => {
    const input = "csc311/ Calc1/ 301"
    const actual = parseOr(input)
    const expected = [["CSC311", "CALC1", "CSC301"], ""]
    recursiveChecker(actual, expected)
  })
  test("parseOr correctly returns parsed course when the last two courses are separated by , or ;", () => {
    const input1 = "csc301/317/,Calc1"
    const actual1 = parseOr(input1)
    const expected1 = [["CSC301", "CSC317"], ",Calc1"]
    recursiveChecker(actual1, expected1)

    const input2 = "csc301/317/;Calc1"
    const actual2 = parseOr(input2)
    const expected2 = [["CSC301", "CSC317"], ";Calc1"]
    recursiveChecker(actual2, expected2)
  })
})

describe("parseCourse", () => {
  test("parseCourse correctly returns an array with a string starting with a prefix", () => {
    const input = "CSC111/207/209/258"
    const actual = parseCourse(input, "CSC")
    const expected = ["CSC111", "/207/209/258"]
    recursiveChecker(actual, expected)
  })
  test("parseCourse correctly returns an array with a string starting without a prefix", () => {
    const input = "207/209/258"
    const actual = parseCourse(input, "CSC")
    const expected = ["CSC207", "/209/258"]
    recursiveChecker(actual, expected)
  })
  test("parseCourse correctly returns an array with a string containing one course number", () => {
    const input = "207"
    const actual = parseCourse(input, "CSC")
    const expected = ["CSC207", ""]
    recursiveChecker(actual, expected)
  })
  
  test("parseCourse correctly returns an array with a string containing one course", () => {
    const input = "CSC209"
    const actual = parseCourse(input, "CSC")
    const expected = ["CSC209", ""]
    recursiveChecker(actual, expected)
  })

  test("parseCourse correctly returns an array with a string with comma as separator", () => {
    const input = "CSC207,209,236"
    const actual = parseCourse(input, "CSC")
    const expected = ["CSC207", ",209,236"]
    recursiveChecker(actual, expected)
  })
  test("parseCourse returns an array containing two empty strings when the input s is empty", () => {
    const input = ""
    const actual = parseCourse(input, "CSC")
    const expected = ["", ""]
    recursiveChecker(actual, expected)
  })
})
