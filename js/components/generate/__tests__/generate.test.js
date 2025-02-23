import React from "react"
import GenerateForm from "../GenerateForm.js"
import { screen, render, waitFor, within } from "@testing-library/react"
import { userEvent } from "@testing-library/user-event"

describe("Handle invalid course inputs appropriately", () => {
  beforeEach(() => {
    render(<GenerateForm />)
  })
  it.each([
    {
      coursesInputText: "MAT777H1, asdasdasd, CSC876Y1",
      expectedWarning: "The course asdasdasd was invalid! Please check your input.",
    },
    {
      coursesInputText: "MAT777H1",
      expectedWarning: "The course MAT777H1 was invalid! Please check your input.",
    },
    {
      coursesInputText: "CSC110Y1, SDS777H1, CSC343H1, MAT888Y1, MAT237Y1",
      expectedWarning:
        "The courses [SDS777H1,MAT888Y1] were invalid! Please check your input.",
    },
    {
      coursesInputText: "CSC110Y1 CSC343H1",
      expectedWarning:
        "The course CSC110Y1 CSC343H1 was invalid! Please check your input.",
    },
    {
      coursesInputText: "",
      expectedWarning: "Cannot generate graph – no courses entered!",
    },
    {
      coursesInputText: "   ",
      expectedWarning: "Cannot generate graph – no courses entered!",
    },
  ])(".$coursesInputText", async ({ coursesInputText, expectedWarning }) => {
    const user = userEvent.setup()
    const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)
    if (coursesInputText === "") {
      coursesInputText = "{Backspace}"
    }
    await user.keyboard(coursesInputText)

    expect(screen.queryByText("Invalid Course Input")).toBeNull()
    const genButton = screen.getByText("Generate Graph")
    await user.click(genButton)
    const warningModal = (await screen.findByText("Invalid Course Input")).parentElement
    expect(warningModal).not.toBeNull()
    const warningMessage = within(warningModal).queryByText(expectedWarning)

    expect(warningMessage).not.toBeNull()
  })
})

describe("Handle invalid department inputs appropriately", () => {
  beforeEach(() => {
    render(<GenerateForm />)
  })

  it.each([
    {
      departmentInputText: "MATH",
      expectedWarning: "The department MATH was invalid! Please check your input.",
    },
    {
      departmentInputText: "CSC, SDS, MAT, PHYS, BIO",
      expectedWarning: "The department PHYS was invalid! Please check your input.",
    },
    {
      departmentInputText: "CSC, MAT STA",
      expectedWarning: "The department MAT STA was invalid! Please check your input.",
    },
  ])(".$departmentInputText", async ({ departmentInputText, expectedWarning }) => {
    const user = userEvent.setup()
    const departmentInputField = screen.getByDisplayValue("CSC, MAT, STA")
    await user.click(departmentInputField)
    await user.tripleClick(departmentInputField)
    if (departmentInputText === "") {
      departmentInputText = "{Backspace}"
    }
    await user.keyboard(departmentInputText)

    const coursesInputText = "CSC443H1"
    const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)
    await user.keyboard(coursesInputText)

    expect(screen.queryByText("Invalid Department Input")).toBeNull()
    const genButton = screen.getByText("Generate Graph")
    await user.click(genButton)
    const warningModal = (await screen.findByText("Invalid Department Input"))
      .parentElement
    expect(warningModal).not.toBeNull()
    const warningMessage = within(warningModal).queryByText(expectedWarning)

    expect(warningMessage).not.toBeNull()
  })
})

describe("Handle invalid taken courses inputs appropriately", () => {
  beforeEach(() => {
    render(<GenerateForm />)
  })

  it.each([
    {
      takenCoursesInputText: "MAT777H1, asdasdasd, CSC876Y1",
      expectedWarning: "The course asdasdasd was invalid! Please check your input.",
    },
    {
      takenCoursesInputText: "CSC110Y1 CSC343H1",
      expectedWarning:
        "The course CSC110Y1 CSC343H1 was invalid! Please check your input.",
    },
  ])(".$takenCoursesInputText", async ({ takenCoursesInputText, expectedWarning }) => {
    const user = userEvent.setup()
    const takenCoursesInputField = screen.getByPlaceholderText(
      "E.g., CSC207H1, CSC236H1"
    )
    await user.click(takenCoursesInputField)
    await user.tripleClick(takenCoursesInputField)
    if (takenCoursesInputText === "") {
      takenCoursesInputText = "{Backspace}"
    }
    await user.keyboard(takenCoursesInputText)

    const coursesInputText = "CSC443H1"
    const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)
    await user.keyboard(coursesInputText)

    expect(screen.queryByText("Invalid Course Input")).toBeNull()
    const genButton = screen.getByText("Generate Graph")
    await user.click(genButton)
    const warningModal = (await screen.findByText("Invalid Course Input")).parentElement
    expect(warningModal).not.toBeNull()
    const warningMessage = within(warningModal).queryByText(expectedWarning)

    expect(warningMessage).not.toBeNull()
  })
})

describe("Exiting the Warning Modal works", () => {
  let user
  beforeEach(async () => {
    user = userEvent.setup()
    render(<GenerateForm />)
    const coursesInputText = "MAT777H1"
    const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)
    await user.keyboard(coursesInputText)
    expect(screen.queryByText("Invalid Course Input")).toBeNull()
    const genButton = screen.getByText("Generate Graph")
    await user.click(genButton)
    let warningModal = (await screen.findByText("Invalid Course Input")).parentElement
    expect(warningModal).not.toBeNull()
  })

  it("`Okay` button works", async () => {
    const okayButton = screen.getByText("Okay")
    expect(okayButton).toBeDefined()
    await user.click(okayButton)
    await waitFor(() => {
      expect(screen.queryByText("Invalid Course Input")).toBeNull()
    })
  })
  it("`X` button works", async () => {
    const XButton = screen.getByText("X")
    expect(XButton).toBeDefined()
    await user.click(XButton)
    await waitFor(() => {
      expect(screen.queryByText("Invalid Course Input")).toBeNull()
    })
  })
})

it("No warning for valid course input strings", async () => {
  const user = userEvent.setup()
  render(<GenerateForm />)
  const coursesInputText = "CSC443H1"
  const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
  await user.click(coursesInputField)
  await user.tripleClick(coursesInputField)
  await user.keyboard(coursesInputText)
  expect(screen.queryByText("Invalid Course Input")).toBeNull()
  const genButton = screen.getByText("Generate Graph")
  await user.click(genButton)
  await expect(screen.findByText("Invalid Course Input")).rejects.toThrow()
})

it("Submitting with valid courses and then making them invalid correctly updates Graph", async () => {
  const user = userEvent.setup()
  render(<GenerateForm />)
  const coursesInputText = "CSC443H1"
  const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
  await user.click(coursesInputField)
  await user.tripleClick(coursesInputField)
  await user.keyboard(coursesInputText)
  expect(screen.queryByText("Invalid Course Input")).toBeNull()
  let genButton = screen.getByText("Generate Graph")
  await user.click(genButton)
  await expect(screen.findByText("Invalid Course Input")).rejects.toThrow()
  expect(screen.getByText("CSC443H1")).toBeDefined()

  // now submitting a bad input to see if the graph gets updated
  const coursesInputTextBad = "CSC443H7"
  await user.click(coursesInputField)
  await user.tripleClick(coursesInputField)
  await user.keyboard(coursesInputTextBad)
  expect(screen.queryByText("Invalid Course Input")).toBeNull()
  await user.click(genButton)
  const warningModal = await screen.findByText("Invalid Course Input")
  expect(warningModal).toBeDefined()
  expect(screen.queryByText("CSC443H1")).toBeNull()
})
