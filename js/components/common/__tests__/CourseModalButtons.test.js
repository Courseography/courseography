import React from "react"
import "@testing-library/jest-dom"
import { CourseModal } from "../react_modal.js.jsx"
import { render, screen } from "@testing-library/react"

describe("CourseModal", () => {
  it("disables back button when there are no previously selected courses", () => {
    render(
      <CourseModal showCourseModal={true} onClose={() => {}} currVisitedIndex={0} />
    )

    const backButton = screen.getByRole("button", { name: /</i })

    expect(backButton).toBeDisabled()
  })

  it("disables forward button when there are no selected courses remaining to go through", () => {
    render(
      <CourseModal
        showCourseModal={true}
        onClose={() => {}}
        currVisitedCourses={["CSC111, CSC110"]}
        currVisitedIndex={1}
      />
    )

    const forwardButton = screen.getByRole("button", { name: /</i })

    expect(forwardButton).toBeDisabled()
  })
})
