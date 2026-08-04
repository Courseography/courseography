// Tests for parsing algorithms from js/util/util.js
import { parseAnd, parseOr, splitPrereqString, removeOuterParens } from "./util"

describe("parseAnd", () => {
  test("parseAnd correctly parses courses when a comma separates two couress", () => {
    const input = "CSC111, STA247"
    const actual = parseAnd(input)
    const expected = ["CSC111", "STA247"]
    expect(actual).toEqual(expected)
  })
  test("parseAnd correctly accounts for shorthand course code expansion", () => {
    const input = "CSC110,111"
    const actual = parseAnd(input)
    const expected = ["CSC110", "CSC111"]
    expect(actual).toEqual(expected)
  })
  test("parseAnd correctly filters out grade requirements", () => {
    const input = "CSC110 (70%),111 (77%)"
    const actual = parseAnd(input)
    const expected = ["CSC110", "CSC111"]
    expect(actual).toEqual(expected)
  })
  test("parseAnd correctly returns parsed course when a string of a single course is wrapped in parentheses", () => {
    const input = "(CSC110)"
    const actual = parseAnd(input)
    const expected = "CSC110"
    expect(actual).toEqual(expected)
  })
  test("parseAnd correctly returns parsed course when the string only contains one course", () => {
    const input = "CSC110"
    const actual = parseAnd(input)
    const expected = "CSC110"
    expect(actual).toEqual(expected)
  })
  test("parseAnd correctly parses courses separated by both comma and slash", () => {
    const input = "CSC111, MAT135/136/137"
    const actual = parseAnd(input)
    const expected = ["CSC111", ["MAT135", "MAT136", "MAT137"]]
    expect(actual).toEqual(expected)
  })
  test("parseAnd correctly parses courses separated by ; and slash together", () => {
    const input = "CSC111/; MAT135/136/137"
    const actual = parseAnd(input)
    const expected = [["CSC111"], ["MAT135", "MAT136", "MAT137"]]
    expect(actual).toEqual(expected)
  })
  test("parseAnd correctly parses courses separated by multiple commas and slash and ;", () => {
    const input = "CSC111, STA247, Calc1/; MAT135/136, CSC145/CSC165/; CSC108/199"
    const actual = parseAnd(input)
    const expected = [
        "CSC111",
        "STA247",
        ["Calc1"],
        ["MAT135", "MAT136"],
        ["CSC145", "CSC165"],
        ["CSC108", "CSC199"],
    ]
    expect(actual).toEqual(expected)
  })
})

describe("parseOr", () => {
  test("parseOr correctly parses courses separated by /", () => {
    const input = "CSC111/CSC165/MAT149"
    const actual = parseOr(input)
    const expected = ["CSC111", "CSC165", "MAT149"]
    expect(actual).toEqual(expected)
  })
  test("parseOr correctly accounts for shorthand course code expansion", () => {
    const input = "CSC111/207/209/258"
    const actual = parseOr(input)
    const expected = ["CSC111", "CSC207", "CSC209", "CSC258"]
    expect(actual).toEqual(expected)
  })
  test("parseOr correctly filters out grade requirements", () => {
    const input = "MAT137(73%) / MAT157(67%)"
    const actual = parseOr(input)
    const expected = ["MAT137", "MAT157"]
    expect(actual).toEqual(expected)
  })
  test("parseOr correctly returns parsed course when a string of a single course is wrapped in parentheses", () => {
    const input = "(CSC207)"
    const actual = parseOr(input)
    const expected = "CSC207"
    expect(actual).toEqual(expected)
  })
  test("parseOr correctly returns parsed course when the string only contains one course", () => {
    const input = "CSC207"
    const actual = parseOr(input)
    const expected = "CSC207"
    expect(actual).toEqual(expected)
  })
  test("parseOr correctly returns a nested conjunction when only a comma split is to be made", () => {
    const input = "CSC207,209"
    const actual = parseOr(input)
    const expected = [["CSC207", "CSC209"]]
    expect(actual).toEqual(expected)
  })
  test("parseOr correctly returns parsed course when there is empty space between some of the courses", () => {
    const input = "CSC311/ 301/ Calc1"
    const actual = parseOr(input)
    const expected = ["CSC311", "CSC301", "Calc1"]
    expect(actual).toEqual(expected)
  })
})

describe("removeOuterParens", () => {
  test("removeOuterParens correctly strips a set of enclosing parentheses around a string", () => {
    const input = "(CSC111/207/209/258)"
    const actual = removeOuterParens(input)
    const expected = "CSC111/207/209/258"
    expect(actual).toEqual(expected)
  })
  test("removeOuterParens does not strip extra nested parentheses inside a string", () => {
    const input = "(CSC111/207/209/258, (MAT149/159), CSC300)"
    const actual = removeOuterParens(input)
    const expected = "CSC111/207/209/258, (MAT149/159), CSC300"
    expect(actual).toEqual(expected)
  })
  test("removeOuterParens does not strip parentheses enclosing only part of a string", () => {
    const input = "(MAT235, MAT236)/MAT237/MAT257"
    const actual = removeOuterParens(input)
    const expected = "(MAT235, MAT236)/MAT237/MAT257"
    expect(actual).toEqual(expected)
  })
  test("removeOuterParens does not strip disjoint sets of parentheses enclosing a string", () => {
    const input = "(MAT235, MAT236)/(MAT237/MAT257)"
    const actual = removeOuterParens(input)
    const expected = "(MAT235, MAT236)/(MAT237/MAT257)"
    expect(actual).toEqual(expected)
  })
  test("removeOuterParens correctly strips multiple layers only when they enclose the string", () => {
    const input = "(((((CSC110)),((CSC111)))))"
    const actual = removeOuterParens(input)
    const expected = "((CSC110)),((CSC111))"
    expect(actual).toEqual(expected)
  })
})

describe("splitPrereqString", () => {
  test("splitPrereqString correctly splits courses by a separator", () => {
    const input = "CSC110,CSC111"
    const actual = splitPrereqString(input, ",")
    const expected = ["CSC110", "CSC111"]
    expect(actual).toEqual(expected)
  })
  test("splitPrereqString correctly filters out spaces when performing a split", () => {
    const input = "CSC110/ CSC111/ CSC207"
    const actual = splitPrereqString(input, "/")
    const expected = ["CSC110", "CSC111", "CSC207"]
    expect(actual).toEqual(expected)
  })
  test("splitPrereqString correctly filters out enclosing parentheses when performing a split", () => {
    const input = "CSC111/207/209/258, (MAT149/159), CSC300"
    const actual = splitPrereqString(input, ",")
    const expected = ["CSC111/207/209/258", "MAT149/159", "CSC300"]
    expect(actual).toEqual(expected)
  })
  test("splitPrereqString does not perform a split inside a parenthesis layer", () => {
    const input = "(MAT235, MAT236)/MAT237/MAT257, CSC111"
    const actual = splitPrereqString(input, ",")
    const expected = ["(MAT235,MAT236)/MAT237/MAT257", "CSC111"]
    expect(actual).toEqual(expected)
  })
})
