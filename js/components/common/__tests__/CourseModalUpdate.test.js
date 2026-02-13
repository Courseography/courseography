import { render, screen, cleanup } from "@testing-library/react"
import { CourseModal } from "../react_modal.js.jsx"
import fetchMock from "fetch-mock"

describe("componentDidUpdate", () => {
  beforeEach(() => {
    cleanup()
    fetchMock.restore()
  })

  afterEach(() => {
    fetchMock.restore()
  })

  it("fetches course data and updates title when a new course is selected", async () => {
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
      allMeetingTimes: [],
      videoUrls: [],
    }

    fetchMock.get("/course?name=CSC110Y1", {
      status: 200,
      body: mockCourseData,
    })

    const { rerender } = render(
      <CourseModal showCourseModal={false} courseId="" onClose={jest.fn()} />
    )

    rerender(
      <CourseModal showCourseModal={true} courseId="CSC110Y1" onClose={jest.fn()} />
    )

    await screen.findByText("CSC110Y1 Foundations of Computer Science I")

    expect(fetchMock.called("/course?name=CSC110Y1")).toBe(true)
  })

  it("handles the case when course is not found", async () => {
    fetchMock.get("/course?name=MISSING110Y1", {
      status: 404,
      body: {},
    })

    const { rerender } = render(
      <CourseModal showCourseModal={false} courseId="" onClose={jest.fn()} />
    )

    rerender(
      <CourseModal showCourseModal={true} courseId="MISSING110Y1" onClose={jest.fn()} />
    )

    await screen.findByText("Course Not Found")
  })
})
