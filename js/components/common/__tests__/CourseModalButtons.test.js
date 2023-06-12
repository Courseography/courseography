import "@testing-library/jest-dom"
import { CourseModal } from "../react_modal.js.jsx"
import * as React from "react"
import { render, screen } from "@testing-library/react"
import userEvent from "@testing-library/user-event"

describe("CourseModal", () => {
  it("disables forward button when course link is clicked", () => {
    render(<CourseModal showCourseModal={true} onClose={() => {}} />)

    const backButton = screen.queryByRole("button", { name: /</i })

    expect(backButton).not.toBeInTheDocument()
    screen.debug()
  })

  //    it("clicking on back button calls the infoModalBackClick callback handler", () => {
  //        const infoModalBackClick = jest.fn()
  //
  //        render(
  //              <CourseModal showCourseModal={true} onClose={() => {}} />
  //            )
  //
  //        const backButton = screen.findByRole('button', { name: /</i })
  //        userEvent.click(backButton)
  //
  //        expect(infoModalBackClick).toHaveBeenCalledTimes(1)
  //    })
  //
  //    it("clicking on forward button calls the infoModalForwardClick callback handler", () => {
  //        const infoModalForwardClick = jest.fn()
  //
  //        render(
  //              <CourseModal showCourseModal={true} onClose={() => {}} />
  //            )
  //
  //        const forwardButton = screen.getByRole('button', { name: />/i })
  //        userEvent.click(forwardButton)
  //
  //        expect(infoModalForwardClick).toHaveBeenCalledTimes(1)
  //    })

  //  it("disables back button when there are no previously selected courses", () => {
  //    render(
  //      <CourseModal showCourseModal={true} onClose={() => {}} currVisitedIndex={0} />
  //    )
  //
  //    const backButton = screen.getByRole("button", { name: /</i })
  //
  //    expect(backButton).toBeDisabled()
  //  })
  //
  //  it("disables forward button when there are no visited courses remaining to go through", () => {
  //    render(
  //      <CourseModal
  //        showCourseModal={true}
  //        onClose={() => {}}
  //        currVisitedCourses={["CSC111, CSC110"]}
  //        currVisitedIndex={1}
  //      />
  //    )
  //
  //    const forwardButton = screen.getByRole("button", { name: />/i })
  //
  //    expect(forwardButton).toBeDisabled()
  //  })
})
