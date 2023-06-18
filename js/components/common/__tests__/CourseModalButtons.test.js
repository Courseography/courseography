import "@testing-library/jest-dom"
import { CourseModal } from "../react_modal.js.jsx"
import * as React from "react"
import { render, screen, userEvent, fireEvent } from "@testing-library/react"
import TestGraph from "../../graph/__tests__/TestGraph.js"

describe("CourseModal", () => {
  it("does not render buttons when course modal is initially opened", async () => {
    await TestGraph.build()

    const node = document.querySelector('[data-testid="aaa100"]')
    fireEvent.mouseOver(node)

    const infobox = document.getElementById("aaa100-tooltip-rect")
    fireEvent.click(infobox)

    // course modal appears, but the buttons are not visible
    expect(document.querySelector(".info-modal-button")).toBeNull
  })

  it("renders buttons when course link is clicked", async () => {
    await TestGraph.build()

    const node = document.querySelector('[data-testid="aaa100"]')
    fireEvent.mouseOver(node)

    const infobox = document.getElementById("aaa100-tooltip-rect")
    fireEvent.click(infobox)

    // course modal appears, but the buttons are not visible
    const link = document.querySelector(".course-selection")
    console.log(link)
    //    fireEvent.click(link)
    //
    //    expect(document.querySelector(".info-modal-button")).toBeNull
  })
})
