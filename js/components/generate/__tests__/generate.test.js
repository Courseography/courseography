import React from "react"
import GenerateForm from "../GenerateForm.js"
import { screen, render } from "@testing-library/react"
import { userEvent } from "@testing-library/user-event"
import fetchMock from "fetch-mock"
import { cleanup } from "@testing-library/react"

describe("Handle invalid course inputs appropriately", () => {
  beforeEach(() => {
    cleanup()
    fetchMock.restore()
    fetchMock.get("/courses", [])
    render(<GenerateForm />)
  })
  it.each([
    {
      coursesInputText: "MAT777H1, asdasdasd, CSC876Y1",
      expectedWarning: "Invalid course code: asdasdasd",
    },
    {
      coursesInputText: "MAT777H1",
      expectedWarning: "Invalid course code: MAT777H1",
    },
    {
      coursesInputText: "CSC110Y1, SDS777H1, CSC343H1, MAT888Y1, MAT237Y1",
      expectedWarning: "Invalid course codes: SDS777H1, MAT888Y1",
    },
    {
      coursesInputText: "CSC110Y1 CSC343H1",
      expectedWarning: "Invalid course code: CSC110Y1 CSC343H1",
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

    expect(screen.queryByText(expectedWarning)).toBeNull()
    const genButton = screen.getByText("Generate")
    await user.click(genButton)

    const errorMessage = await screen.findByText(expectedWarning)
    expect(errorMessage).not.toBeNull()
  })
  afterEach(() => {
    fetchMock.restore()
  })
})

describe("Handle invalid department inputs appropriately", () => {
  beforeEach(() => {
    render(<GenerateForm />)
  })

  it.each([
    {
      departmentInputText: "CSC, SDS, MAT, PHYS, BIO",
      expectedWarning: "Invalid department: PHYS",
    },
    {
      departmentInputText: "csc, abcd, SDS, MAT, PHYS, BIO",
      expectedWarning: "Invalid departments: abcd, PHYS",
    },
    {
      departmentInputText: "CSC, MAT STA",
      expectedWarning: "Invalid department: MAT STA",
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

    expect(screen.queryByText(expectedWarning)).toBeNull()
    const genButton = screen.getByText("Generate")
    await user.click(genButton)

    const errorMessage = await screen.findByText(expectedWarning)
    expect(errorMessage).not.toBeNull()
  })
})

describe("Handle invalid taken courses inputs appropriately", () => {
  beforeEach(() => {
    render(<GenerateForm />)
  })

  it.each([
    {
      takenCoursesInputText: "MAT777H1, asdasdasd, CSC876Y1",
      expectedWarning: "Invalid course code: asdasdasd",
    },
    {
      takenCoursesInputText: "CSC110Y1 CSC343H1",
      expectedWarning: "Invalid course code: CSC110Y1 CSC343H1",
    },
    {
      takenCoursesInputText: "MAT1234H1, CSC207H1, CSC1234Y1",
      expectedWarning: "Invalid course codes: MAT1234H1, CSC1234Y1",
    },
  ])(".$takenCoursesInputText", async ({ takenCoursesInputText, expectedWarning }) => {
    const user = userEvent.setup()
    const takenCoursesInputField = screen.getByPlaceholderText(
      "e.g., CSC207H1, CSC236H1"
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

    expect(screen.queryByText(expectedWarning)).toBeNull()
    const genButton = screen.getByText("Generate")
    await user.click(genButton)

    const errorMessage = await screen.findByText(expectedWarning)
    expect(errorMessage).not.toBeNull()
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
  const genButton = screen.getByText("Generate")
  await user.click(genButton)
  await expect(screen.findByText(/invalid/i)).rejects.toThrow()
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
  let genButton = screen.getByText("Generate")
  await user.click(genButton)
  await expect(screen.findByText("Invalid Course Input")).rejects.toThrow()
  expect(screen.getByText("CSC443H1")).toBeDefined()

  // now submitting a bad input to see if the graph gets updated
  const coursesInputTextBad = "CSC443H7"
  await user.click(coursesInputField)
  await user.tripleClick(coursesInputField)
  await user.keyboard(coursesInputTextBad)
  expect(screen.queryByText(/invalid/i)).toBeNull()
  await user.click(genButton)
  const errorMessage = await screen.findByText(/Invalid course code: CSC443H7/i)
  expect(errorMessage).toBeDefined()
  expect(screen.queryByText("CSC443H1")).toBeNull()
})

describe("Handle invalid program inputs appropriately", () => {
  beforeEach(() => {
    render(<GenerateForm />)
  })
  it.each([
    {
      programInputText: "ASMAJ1234, asdasdasd, ABCDE1234",
      expectedWarning: "Invalid program code: asdasdasd",
    },
    {
      programInputText: "ASMIN1689 ASFOC1689F",
      expectedWarning: "Invalid program code: ASMIN1689 ASFOC1689F",
    },
    {
      programInputText: "",
      expectedWarning: "Cannot generate graph – no programs entered!",
    },
    {
      programInputText: "   ",
      expectedWarning: "Cannot generate graph – no programs entered!",
    },
  ])(".$programInputText", async ({ programInputText, expectedWarning }) => {
    const user = userEvent.setup()

    const categorySelect = screen.getByRole("combobox")
    await user.selectOptions(categorySelect, "programs")

    const programsInputField = screen.getByPlaceholderText(
      "e.g., ASMAJ1689, ASFOC1689B"
    )
    await user.click(programsInputField)
    await user.tripleClick(programsInputField)
    if (programInputText === "") {
      programInputText = "{Backspace}"
    }
    await user.keyboard(programInputText)

    expect(screen.queryByText(expectedWarning)).toBeNull()
    const genButton = screen.getByText("Generate")
    await user.click(genButton)

    const errorMessage = await screen.findByText(expectedWarning)
    expect(errorMessage).not.toBeNull()
  })
})

it("No warning for valid program input strings", async () => {
  const user = userEvent.setup()
  render(<GenerateForm />)

  const categorySelect = screen.getByRole("combobox")
  await user.selectOptions(categorySelect, "programs")

  const programInputText = "ASFOC1689D"
  const programsInputField = screen.getByPlaceholderText("e.g., ASMAJ1689, ASFOC1689B")
  await user.click(programsInputField)
  await user.tripleClick(programsInputField)
  await user.keyboard(programInputText)
  expect(screen.queryByText("Invalid Program Input")).toBeNull()
  const genButton = screen.getByText("Generate")
  await user.click(genButton)
  await expect(screen.findByText(/invalid/i)).rejects.toThrow()
})
