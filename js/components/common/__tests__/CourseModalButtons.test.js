import { screen, waitFor, fireEvent } from "@testing-library/react"
import TestGraph from "../../graph/__tests__/TestGraph.js"

describe("CourseModal", () => {
  beforeEach(async () => {
    await TestGraph.build()

    // find the course node aa100
    const node = document.querySelector('[data-testid="aaa100"]')
    fireEvent.mouseOver(node)

    // find the infobox beside it, and click on it
    const infobox = document.getElementById("aaa100-tooltip-rect")
    fireEvent.click(infobox)
  })

  it("does not render buttons when course modal is initially opened", async () => {
    expect(document.querySelector(".ReactModal__Body--open")).not.toBeNull
    expect(document.querySelector(".info-modal-button")).toBeNull
  })

  it("renders buttons when course link is clicked", async () => {
    // find the modal
    await waitFor(() => {
      const modal = document.querySelector(".ReactModalPortal")
      return modal !== null
    })

    // find the link
    const link = screen.getByText("BBB100H1")
    fireEvent.click(link)

    // find the buttons
    const buttons = document.querySelectorAll(".info-modal-button")
    expect(buttons.length == 2).toBe(true)

    // check that forward button is disabled, back button is enabled
    const forward_button = screen.getByText(">")
    const back_button = screen.getByText("<")
    expect(forward_button.disabled).toBe(true)
    expect(back_button.disabled).toBe(false)
  })
})
