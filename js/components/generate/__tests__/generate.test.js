import React from "react"
import GenerateForm from "../GenerateForm.js"
import { screen, render, fireEvent, waitFor, within } from "@testing-library/react"
import courseToResp from "../__mocks__/sample_responses.json"

const origFetch = global.fetch

const mocker = courseInput => {
  let resp = courseToResp[courseInput]
  return jest.fn(() =>
    Promise.resolve({
      json: () => Promise.resolve(resp),
    })
  )
}

describe("Handle invalid course inputs appropriately", () => {
  beforeEach(() => {
    render(<GenerateForm />)
  })
  afterEach(() => {
    global.fetch = origFetch
  })
  it.each([
    {
      coursesInputText: "MAT777H1, asdasdasd, CSC876Y1",
      expectedWarning:
        "The courses [MAT777H1,asdasdasd,CSC876Y1] were invalid! Please check your input.",
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
      coursesInputText: "",
      expectedWarning: "Cannot generate graph – no courses entered!",
    },
    {
      coursesInputText: "   ",
      expectedWarning: "Cannot generate graph – no courses entered!",
    },
  ])(".$coursesInputText", async ({ coursesInputText, expectedWarning }) => {
    global.fetch = mocker(coursesInputText)
    const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
    fireEvent.change(coursesInputField, { target: { value: coursesInputText } })
    expect(screen.queryByText("Invalid Course Input")).toBeNull()
    const genButton = screen.getByText("Generate Graph")
    fireEvent.click(genButton)
    const warningModal = (await screen.findByText("Invalid Course Input")).parentElement
    expect(warningModal).not.toBeNull()
    // screen.debug(warningModal)
    const warningMessage = within(warningModal).queryByText(expectedWarning)

    expect(warningMessage).not.toBeNull()
  })
})

describe("Exiting the Warning Modal works", () => {
  beforeEach(async () => {
    render(<GenerateForm />)
    const coursesInputText = "MAT777H1"
    global.fetch = mocker(coursesInputText)
    const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
    fireEvent.change(coursesInputField, { target: { value: coursesInputText } })
    expect(screen.queryByText("Invalid Course Input")).toBeNull()
    const genButton = screen.getByText("Generate Graph")
    fireEvent.click(genButton)
    screen.debug()
    let warningModal = (await screen.findByText("Invalid Course Input")).parentElement
    expect(warningModal).not.toBeNull()
  })

  it("`Okay` button works", async () => {
    const okayButton = screen.getByText("Okay")
    expect(okayButton).toBeDefined()
    fireEvent.click(okayButton)
    await waitFor(() => {
      expect(screen.queryByText("Invalid Course Input")).toBeNull()
    })
  })
  it("`X` button works", async () => {
    const XButton = screen.getByText("X")
    expect(XButton).toBeDefined()
    fireEvent.click(XButton)
    await waitFor(() => {
      expect(screen.queryByText("Invalid Course Input")).toBeNull()
    })
    screen.debug()
  })
})

it("No warning for valid course input strings", async () => {
  render(<GenerateForm />)
  global.fetch = mocker(coursesInputText)
  const coursesInputText = "CSC443H1"
  const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
  fireEvent.change(coursesInputField, { target: { value: coursesInputText } })
  expect(screen.queryByText("Invalid Course Input")).toBeNull()
  const genButton = screen.getByText("Generate Graph")
  fireEvent.click(genButton)
  await expect(screen.findByText("Invalid Course Input")).rejects.toThrow()
})

it("Submitting with valid courses and then making them invalid correctly updates Graph", async () => {
  render(<GenerateForm />)
  const coursesInputText = "CSC443H1"
  global.fetch = mocker(coursesInputText)
  const coursesInputField = screen.getByPlaceholderText("e.g., CSC207H1, CSC324H1")
  fireEvent.change(coursesInputField, { target: { value: coursesInputText } })
  expect(screen.queryByText("Invalid Course Input")).toBeNull()
  let genButton = screen.getByText("Generate Graph")
  fireEvent.click(genButton)
  await expect(screen.findByText("Invalid Course Input")).rejects.toThrow()
  expect(screen.getByText("CSC443H1")).toBeDefined()

  // now submitting a bad input to see if the graph gets updated
  const coursesInputTextBad = "CSC443H7"
  fireEvent.change(coursesInputField, { target: { value: coursesInputTextBad } })
  expect(screen.queryByText("Invalid Course Input")).toBeNull()
  fireEvent.click(genButton)
  const warningModal = await screen.findByText("Invalid Course Input")
  expect(warningModal).toBeDefined()
  expect(screen.getByText("CSC443H1")).toBeDefined()
})
