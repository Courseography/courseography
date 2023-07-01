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

  it("renders disabled buttons when course modal first opens", async () => {
    await waitForModalUpdate()

    // the back and forward buttons render
    const buttons = document.querySelectorAll(".info-modal-button")
    expect(buttons.length == 2).toBe(true)

    // the back and forward buttons are disabled
    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    expect(forwardButton.disabled).toBe(true)
    expect(backButton.disabled).toBe(true)
  })

  it("enables back button when a course link is clicked", async () => {
    await waitForModalUpdate()

    // click on the course link BBB100H1, and BBB100H1's modal opens
    const bbbCourseLink = screen.getByText("BBB100H1")
    fireEvent.click(bbbCourseLink)

    await waitForModalUpdate()

    // the forward button is disabled, but the back button is enabled
    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    expect(forwardButton.disabled).toBe(true)
    expect(backButton.disabled).toBe(false)
  })

  it("enables forward button when user clicks on course link, and goes back", async () => {
    await waitForModalUpdate()

    // click on the course link BBB100H1, and BBB100H1's modal opens
    const bbbCourseLink = screen.getByText("BBB100H1")
    fireEvent.click(bbbCourseLink)

    await waitForModalUpdate()

    // click on the back button, and AAA100's modal opens
    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    fireEvent.click(backButton)

    await waitForModalUpdate()

    // the forward button is enabled but the back button is disabled
    expect(forwardButton.disabled).toBe(false)
    expect(backButton.disabled).toBe(true)
  })

  it("enables buttons when user clicks on a course link, then clicks on another course link, and goes back", async () => {
    await waitForModalUpdate()

    // click on the course link BBB100H1, and BBB100H1's modal opens
    const bbbCourseLink = screen.getByText("BBB100H1")
    fireEvent.click(bbbCourseLink)

    await waitForModalUpdate()

    // click on the course link CCC100H1, and CCC100H1's modal opens
    const cccCourseLink = screen.getByText("CCC100H1")
    fireEvent.click(cccCourseLink)

    await waitForModalUpdate()

    // click on the back button, and BBB100's modal opens
    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    fireEvent.click(backButton)

    await waitForModalUpdate()

    // both buttons are enabled
    expect(forwardButton.disabled).toBe(false)
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

  it("the buttons function like back and forward buttons in a browser", async () => {
    await waitForModalUpdate()

    // click on the course link BBB100H1, and BBB100H1's modal opens. The user's
    // history of courses looks like: [AAA100, BBB100], with the user currently
    // on BBB100
    const bbbCourseLink = screen.getByText("BBB100H1")
    fireEvent.click(bbbCourseLink)
    await waitForModalUpdate()
    let modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("BBB100H1 Introduction to BBB Thinking")

    // click on the back button, and AAA100H1's modal appears. The user's
    // history of courses looks like: [AAA100, BBB100], with the user currently
    // on AAA100
    const backButton = screen.getByText("<")
    fireEvent.click(backButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("AAA100 Introduction to AAA Thinking")

    // click on the course link CCC100H1, and CCC100H1's modal opens. The user's
    // history of courses looks like: [AAA100, CCC100], with the user currently
    // on CCC100
    const cccCourseLink = screen.getByText("CCC100H1")
    fireEvent.click(cccCourseLink)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("CCC100H1 Introduction to CCC Thinking")

    // click on the back button, and AAA100's modal should open. The user's
    // history of courses looks like: [AAA100, CCC100], with the user currently
    // on AAA100
    fireEvent.click(backButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("AAA100 Introduction to AAA Thinking")

    // click on the forward button, and CCC100's modal should open. The user's
    // history of courses looks like: [AAA100, CCC100], with the user currently
    // on CCC100.
    const forwardButton = screen.getByText(">")
    fireEvent.click(forwardButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("CCC100H1 Introduction to CCC Thinking")

    // the forward button is disabled, and the back button is enabled
    expect(forwardButton.disabled).toBe(true)
    expect(backButton.disabled).toBe(false)
  })
})
