import { screen, waitFor, fireEvent } from "@testing-library/react"
import TestGraph from "../../graph/__tests__/TestGraph.js"

describe("CourseModal", () => {
  beforeEach(async () => {
    await TestGraph.build()

    const node = document.querySelector('[data-testid="aaa100"]')
    fireEvent.mouseOver(node)
    const infobox = document.getElementById("aaa100-tooltip-rect")
    fireEvent.click(infobox)
  })

  const waitForModalUpdate = async () => {
    await waitFor(() => document.querySelector(".ReactModalPortal"))
  }

  it("renders disabled buttons when course modal first opens", async () => {
    await waitForModalUpdate()

    const buttons = document.querySelectorAll(".info-modal-button")
    expect(buttons.length == 2).toBe(true)

    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    expect(forwardButton.disabled).toBe(true)
    expect(backButton.disabled).toBe(true)
  })

  it("enables back button when a course link is clicked", async () => {
    await waitForModalUpdate()

    const bbbCourseLink = screen.getByText("BBB100H1")
    fireEvent.click(bbbCourseLink)

    await waitForModalUpdate()

    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    expect(forwardButton.disabled).toBe(true)
    expect(backButton.disabled).toBe(false)
  })

  it("enables forward button when user clicks on course link, and goes back", async () => {
    await waitForModalUpdate()

    const bbbCourseLink = screen.getByText("BBB100H1")
    fireEvent.click(bbbCourseLink)

    await waitForModalUpdate()

    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    fireEvent.click(backButton)

    await waitForModalUpdate()

    expect(forwardButton.disabled).toBe(false)
    expect(backButton.disabled).toBe(true)
  })

  it("enables buttons when user clicks on a course link, then clicks on another course link, and goes back", async () => {
    await waitForModalUpdate()

    const bbbCourseLink = screen.getByText("BBB100H1")
    fireEvent.click(bbbCourseLink)

    await waitForModalUpdate()

    const cccCourseLink = screen.getByText("CCC100H1")
    fireEvent.click(cccCourseLink)

    await waitForModalUpdate()

    const forwardButton = screen.getByText(">")
    const backButton = screen.getByText("<")
    fireEvent.click(backButton)

    await waitForModalUpdate()

    expect(forwardButton.disabled).toBe(false)
    expect(backButton.disabled).toBe(false)
  })

  it("allows users to navigate back and forward through viewed courses", async () => {
    await waitForModalUpdate()

    const courseLink = screen.getByText("BBB100H1")
    fireEvent.click(courseLink)

    await waitForModalUpdate()
    let modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("BBB100H1 Introduction to BBB Thinking")

    const backButton = screen.getByText("<")
    fireEvent.click(backButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("AAA100 Introduction to AAA Thinking")

    const forwardButton = screen.getByText(">")
    fireEvent.click(forwardButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("BBB100H1 Introduction to BBB Thinking")
  })

  it("the buttons function like back and forward buttons in a browser", async () => {
    await waitForModalUpdate()

    const bbbCourseLink = screen.getByText("BBB100H1")
    fireEvent.click(bbbCourseLink)
    await waitForModalUpdate()
    let modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("BBB100H1 Introduction to BBB Thinking")

    const backButton = screen.getByText("<")
    fireEvent.click(backButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("AAA100 Introduction to AAA Thinking")

    const cccCourseLink = screen.getByText("CCC100H1")
    fireEvent.click(cccCourseLink)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("CCC100H1 Introduction to CCC Thinking")

    fireEvent.click(backButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("AAA100 Introduction to AAA Thinking")

    const forwardButton = screen.getByText(">")
    fireEvent.click(forwardButton)
    await waitForModalUpdate()
    modalHeader = document.querySelector(".modal-header")
    expect(modalHeader.textContent).toContain("CCC100H1 Introduction to CCC Thinking")

    expect(forwardButton.disabled).toBe(true)
    expect(backButton.disabled).toBe(false)
  })
})
