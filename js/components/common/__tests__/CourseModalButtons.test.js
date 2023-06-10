import React from "react"
import { CourseModal } from "../react_modal.js.jsx"
import { render, screen } from "@testing-library/react"

describe("CourseModal", () => {
  it("renders back and forward buttons", () => {
    render(<CourseModal />)

    screen.debug()
  })
})
