import { screen, waitFor, fireEvent } from "@testing-library/react"
import TestGraph from "../../graph/__tests__/TestGraph.js"

describe("CourseModal", () => {
  beforeEach(async () => {
    await TestGraph.build()

    // find the course node AAA100 and open its info modal
    const node = document.querySelector('[data-testid="aaa100"]')
    fireEvent.mouseOver(node)
    const infobox = document.getElementById("aaa100-tooltip-rect")
    fireEvent.click(infobox)
  })

  // this function encapsulates that waiting logic for the modal updates
  const waitForModalUpdate = async () => {
    await waitFor(() => {
      const updatedModal = document.querySelector(".ReactModalPortal")
      return updatedModal
    })
  }

  it("does not render buttons when course modal is initially opened", async () => {
    expect(document.querySelector(".ReactModal__Body--open")).not.toBeNull
    expect(document.querySelector(".info-modal-button")).toBeNull
  })

  it("renders buttons when a course link is clicked", async () => {
    await waitForModalUpdate()
    const courseLink = screen.getByText("BBB100H1")
    fireEvent.click(courseLink)

    // check that two buttons have been rendered
    const buttons = document.querySelectorAll(".info-modal-button")
    expect(buttons.length == 2).toBe(true)

    // check that forward button is disabled, back button is enabled
    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    expect(forwardButton.disabled).toBe(true)
    expect(backButton.disabled).toBe(false)
  })

  it("takes user back to the previously viewed course when back button is clicked", async () => {
    await waitForModalUpdate()
    const courseLink = screen.getByText("BBB100H1")
    fireEvent.click(courseLink)

    // check that BBB100H1 info has opened
    await waitForModalUpdate()
    let modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("BBB100H1 Introduction to BBB Thinking")

    // check that AAA100H1 info is displayed after back button click
    const backButton = screen.getByText("<")
    fireEvent.click(backButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("AAA100 Introduction to AAA Thinking")
  })
})
