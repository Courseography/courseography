import { render, screen, cleanup, waitFor } from "@testing-library/react"
import userEvent from "@testing-library/user-event"
import { CoursePanel } from "../course_panel.js.jsx"
import fetchMock from "fetch-mock"

describe("CoursePanel", () => {
  const coursePanelProps = {
    selectedCourses: [],
    selectedLectures: [],
    removeCourse: jest.fn(),
    clearCourses: jest.fn(),
    hoverLecture: jest.fn(),
    unhoverLecture: jest.fn(),
    selectLecture: jest.fn(),
  }

  beforeEach(() => {
    cleanup()
    fetchMock.restore()
  })

  afterEach(() => {
    fetchMock.restore()
  })

  it("fetches course sections and updates course info", async () => {
    const user = userEvent.setup()
    fetchMock.get("/courses", "CSC110Y1\nCSC111H1\nMAT137Y1")

    const mockCourseData = {
      name: "CSC110Y1",
      title: "Foundations of Computer Science I",
      description: null,
      prereqs: null,
      exclusions: null,
      breadth: null,
      distribution: null,
      prereqString: null,
      coreqs: null,
      allMeetingTimes: [
        {
          meetData: { session: "F", section: "LEC0101", code: "CSC110Y1" },
          timeData: [],
        },
      ],
      videoUrls: [],
    }

    fetchMock.get("/course?name=CSC110Y1", {
      status: 200,
      body: mockCourseData,
    })

    render(<CoursePanel {...coursePanelProps} selectedCourses={["CSC110Y1"]} />)

    const courseHeader = await screen.findByText("CSC110Y1")
    await user.click(courseHeader)

    await screen.findByText("L0101")
  })

  it("handles the case when course is not found", async () => {
    const user = userEvent.setup()
    const consoleErrorMock = jest.spyOn(console, "error").mockImplementation()

    fetchMock.get("/courses", "CSC110Y1\nCSC111H1\nMAT137Y1")

    fetchMock.get("/course?name=MISSING110Y1", {
      status: 404,
      body: {},
    })

    render(<CoursePanel {...coursePanelProps} selectedCourses={["MISSING110Y1"]} />)

    const courseHeader = await screen.findByText("MISSING110Y1")
    await user.click(courseHeader)

    expect(screen.queryByText("L0101")).toBeNull()

    await waitFor(() => {
      expect(consoleErrorMock).toHaveBeenCalledWith(
        expect.stringContaining("Course with code MISSING110Y1 not found")
      )
    })

    consoleErrorMock.mockRestore()
  })
})
