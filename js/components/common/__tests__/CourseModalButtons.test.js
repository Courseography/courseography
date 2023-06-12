import "@testing-library/jest-dom"
import { CourseModal } from "../react_modal.js.jsx"
import * as React from "react"
import { render, screen } from "@testing-library/react"
import userEvent from "@testing-library/user-event"

describe("CourseModal", () => {
  it("does not render buttons when course modal is opened", () => {
    render(<CourseModal showCourseModal={true} onClose={() => {}} />)

    const backButton = screen.queryByRole("button", { name: /</i })
    const forwardButton = screen.queryByRole("button", { name: />/i })

    expect(backButton).not.toBeInTheDocument()
    expect(forwardButton).not.toBeInTheDocument()
    screen.debug()
  })

  it("renders buttons when a course link is clicked for the first time", () => {
    // Render the CSC111 course specific course modal.
    render(<CourseModal showCourseModal={true} courseId="csc111" onClose={() => {}} />)
    screen.debug()

    // Find a course link
    // const link = screen.getByRole("link", { ... })
    userEvent.click(link)

    // Find the buttons
    const backButton = screen.queryByRole("button", { name: /</i })
    const forwardButton = screen.queryByRole("button", { name: />/i })

    expect(backButton).toBeInTheDocument()
    expect(forwardButton).toBeInTheDocument()
  })
})
