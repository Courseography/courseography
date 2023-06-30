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

  // wait for the modal to appear
  const waitForModalUpdate = async () => {
    await waitFor(() => document.querySelector(".ReactModalPortal"))
  }

  it("renders buttons when a course link is clicked", async () => {
    await waitForModalUpdate()

    // click on the course link BBB100H1
    const courseLink = screen.getByText("BBB100H1")
    fireEvent.click(courseLink)

    // the back and forward buttons render
    const buttons = document.querySelectorAll(".info-modal-button")
    expect(buttons.length == 2).toBe(true)

    // the forward button is disabled, but the back button is enabled
    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    expect(forwardButton.disabled).toBe(true)
    expect(backButton.disabled).toBe(false)
  })

  it("allows users to navigate back and forward through viewed courses", async () => {
    await waitForModalUpdate()

    // click on the course link BBB100H1
    const courseLink = screen.getByText("BBB100H1")
    fireEvent.click(courseLink)

    // BBB100H1's modal opens
    await waitForModalUpdate()
    let modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("BBB100H1 Introduction to BBB Thinking")

    // click on the back button, and AAA100H1's modal appears
    const backButton = screen.getByText("<")
    fireEvent.click(backButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("AAA100 Introduction to AAA Thinking")

    // click on the forward button, and BBB100's modal appears
    const forwardButton = screen.getByText(">")
    fireEvent.click(forwardButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("BBB100H1 Introduction to BBB Thinking")
  })
})
