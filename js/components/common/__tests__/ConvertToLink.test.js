import React from "react"
import { convertToLink } from "../CourseModal"

/**
 * Test for convertToLink.
 */
describe("ConvertToLink", () => {
  let actual
  let content
  let expected

  describe("The content has no course names", () => {
    beforeEach(() => {
      content = "The application of logic and proof techniques."
    })
    test("The content is returned as all strings", () => {
      actual = convertToLink(content)
      expected = [content]
      expect(actual).toEqual(expected)
    })
  })

  describe("The content has one course name", () => {
    beforeEach(() => {
      content = "Certain topics briefly mentioned in CSC165H1 may be covered."
    })
    test("The content is returned as one link tag and several strings", () => {
      actual = convertToLink(content)
      expected = [
        "Certain topics briefly mentioned in ",
        <a
          key="1"
          className="course-selection"
          onClick={() => this.clickCourseLink("CSC165H1")}
        >
          CSC165H1
        </a>,
        " may be covered.",
      ]
      expect(JSON.stringify(actual)).toEqual(JSON.stringify(expected))
    })
  })

  describe("The content has one course name followed by a symbol, such as ')' ", () => {
    beforeEach(() => {
      content = "(60% or higher in CSC111H1)"
    })
    test("The content is returned as one link tag and several strings \
         (including the symbol after the course name", () => {
      actual = convertToLink(content)
      expected = [
        "(60% or higher in ",
        <a
          key="1"
          className="course-selection"
          onClick={() => this.clickCourseLink("CSC111H1")}
        >
          CSC111H1
        </a>,
        ")",
      ]
      expect(JSON.stringify(actual)).toEqual(JSON.stringify(expected))
    })
  })
})
