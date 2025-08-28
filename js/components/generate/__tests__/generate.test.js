import React from "react"
import GenerateForm from "../GenerateForm.js"
import { screen, render } from "@testing-library/react"
import { userEvent } from "@testing-library/user-event"
import fetchMock from "fetch-mock"
import { cleanup } from "@testing-library/react"

describe("Handle an incorrect course input appropriately", () => {
  beforeEach(() => {
    cleanup()
    fetchMock.restore()
    fetchMock.get("/courses", "")
    render(<GenerateForm />)
  })

  it("Entering no text should return an error", async () => {
    const user = userEvent.setup()
    const genButton = screen.getByText("Generate")
    await user.click(genButton)
    const errorMessage = await screen.findByText(
      "Cannot generate graph – no courses entered!"
    )
    expect(errorMessage).not.toBeNull()
  })

  it("Entering blank text should return an error", async () => {
    const user = userEvent.setup()
    const coursesInputField = screen.getByRole("combobox", { name: "courses" })
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)

    await user.keyboard("  ")

    expect(screen.queryByText("Cannot generate graph – no courses entered!")).toBeNull()

    const genButton = screen.getByText("Generate")
    await user.click(genButton)
    const errorMessage = await screen.findByText(
      "Cannot generate graph – no courses entered!"
    )
    expect(errorMessage).not.toBeNull()
  })

  it("Entering a course but not pressing it in the selection should return an error", async () => {
    const user = userEvent.setup()
    const coursesInputField = screen.getByRole("combobox", { name: "courses" })
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)

    await user.keyboard("CSC343H1")

    expect(screen.queryByText("Cannot generate graph – no courses entered!")).toBeNull()

    const genButton = screen.getByText("Generate")
    await user.click(genButton)
    const errorMessage = await screen.findByText(
      "Cannot generate graph – no courses entered!"
    )
    expect(errorMessage).not.toBeNull()
  })

  it("Pressing enter after an invalid course should return an empty error not an incorrect course error", async () => {
    const user = userEvent.setup()
    const coursesInputField = screen.getByRole("combobox", { name: "courses" })
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)

    await user.keyboard("CSC898H1{enter}")

    const errorMessage = await screen.findByText(
      "Cannot generate graph – no courses entered!"
    )
    expect(errorMessage).not.toBeNull()
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
    const departmentInputField = screen.getByPlaceholderText("e.g., CSC, MAT, STA")
    await user.click(departmentInputField)
    await user.tripleClick(departmentInputField)
    if (departmentInputText === "") {
      departmentInputText = "{Backspace}"
    }
    await user.keyboard(departmentInputText)

    const coursesInputField = screen.getByRole("combobox", { name: "courses" })
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)

    await user.keyboard("CSC110{ArrowDown}{enter}")

    const genButton = screen.getByText("Generate")
    await user.click(genButton)

    const errorMessage = await screen.findByText(expectedWarning)
    expect(errorMessage).not.toBeNull()
  })
})

it("No warning for valid course input strings", async () => {
  const user = userEvent.setup()
  render(<GenerateForm />)
  const coursesInputField = screen.getByRole("combobox", { name: "courses" })
  await user.click(coursesInputField)
  await user.tripleClick(coursesInputField)
  await user.keyboard("CSC443{ArrowDown}{enter}")
  expect(screen.queryByText("Invalid Course Input")).toBeNull()
  const genButton = screen.getByText("Generate")
  await user.click(genButton)
  await expect(screen.findByText(/invalid/i)).rejects.toThrow()
})

describe("Handle an incorrect course input appropriately", () => {
  beforeEach(() => {
    cleanup()
    fetchMock.restore()
    fetchMock.get("/courses", "")
    render(<GenerateForm />)
  })

  it("Entering no text should return an error", async () => {
    const user = userEvent.setup()
    const genButton = screen.getByText("Generate")
    await user.click(genButton)
    const errorMessage = await screen.findByText(
      "Cannot generate graph – no courses entered!"
    )
    expect(errorMessage).not.toBeNull()
  })

  it("Entering blank text should return an error", async () => {
    const user = userEvent.setup()
    const coursesInputField = screen.getByRole("combobox", { name: "courses" })
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)

    await user.keyboard("  ")

    expect(screen.queryByText("Cannot generate graph – no courses entered!")).toBeNull()

    const genButton = screen.getByText("Generate")
    await user.click(genButton)
    const errorMessage = await screen.findByText(
      "Cannot generate graph – no courses entered!"
    )
    expect(errorMessage).not.toBeNull()
  })

  it("Entering a course but not pressing it in the selection should return an error", async () => {
    const user = userEvent.setup()
    const coursesInputField = screen.getByRole("combobox", { name: "courses" })
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)

    await user.keyboard("CSC343H1")

    expect(screen.queryByText("Cannot generate graph – no courses entered!")).toBeNull()

    const genButton = screen.getByText("Generate")
    await user.click(genButton)
    const errorMessage = await screen.findByText(
      "Cannot generate graph – no courses entered!"
    )
    expect(errorMessage).not.toBeNull()
  })

  it("Pressing enter after an invalid course should return an empty error not an incorrect course error", async () => {
    const user = userEvent.setup()
    const coursesInputField = screen.getByRole("combobox", { name: "courses" })
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)

    await user.keyboard("CSC898H1{enter}")

    const errorMessage = await screen.findByText(
      "Cannot generate graph – no courses entered!"
    )
    expect(errorMessage).not.toBeNull()
  })
})

describe("Handle invalid department inputs appropriately", () => {
  beforeEach(() => {
    fetchMock.restore()
    fetchMock.get("/courses", "CSC443H1\nLIN229H1")
    render(<GenerateForm />)
  })

  it("Submitting with valid courses and then making them invalid correctly updates Graph", async () => {
    const user = userEvent.setup()
    const coursesInputText = "CSC443H1"
    const coursesInputField = screen.getByRole("combobox", { name: "courses" })
    const departmentInputField = screen.getByPlaceholderText("e.g., CSC, MAT, STA")
    await user.click(departmentInputField)
    // limit generated courses to only CSC courses
    await user.keyboard("CSC")
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)
    await user.keyboard(coursesInputText)

    console.log(coursesInputField.value)

    await user.keyboard("{ArrowDown}{enter}")

    expect(screen.queryByText("Invalid Course Input")).toBeNull()
    let genButton = screen.getByText("Generate")
    await user.click(genButton)
    console.log(coursesInputField.value)
    await expect(screen.findByText("Invalid Course Input")).rejects.toThrow()

    await expect(screen.findByText("Selected Courses: CSC443H1")).toBeDefined()

    // now submitting a bad input to see if the graph gets updated
    const coursesInputTextBad = "LIN229H1"
    await user.click(coursesInputField)
    await user.tripleClick(coursesInputField)
    await user.keyboard(coursesInputTextBad)
    await user.keyboard("{ArrowDown}{enter}")
    expect(screen.queryByText(/invalid/i)).toBeNull()
    await user.click(genButton)
    const errorMessage = screen.queryByText(/Invalid course code: LIN229H1/i)
    const twoSelectedMessage = screen.queryByText(
      "Selected Courses: CSC443H1, LIN229H1"
    )
    expect(twoSelectedMessage).toBeDefined
    expect(errorMessage).toBeDefined()
  })
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

    const categorySelect = screen.getByDisplayValue("Courses")
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

  const categorySelect = screen.getByDisplayValue("Courses")
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
