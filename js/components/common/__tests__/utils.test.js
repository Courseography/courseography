import fetchMock from "fetch-mock"
import { getCourse } from "../utils"

describe("getCourse (using fetch-mock)", () => {
  afterEach(() => {
    fetchMock.restore()
  })

  it("successfully returns course data when found", async () => {
    const mockCourseData = {
      name: "CSC110Y1",
      title: "Foundations of Computer Science I",
    }

    fetchMock.get("/course?name=CSC110Y1", {
      status: 200,
      body: mockCourseData,
    })

    const result = await getCourse("CSC110Y1")

    expect(result).toEqual(mockCourseData)
    expect(fetchMock.called("/course?name=CSC110Y1")).toBe(true)
  })

  it("throws an error when the course is not found", async () => {
    fetchMock.get("/course?name=MISSING110Y1", {
      status: 404,
      body: {},
    })

    await expect(getCourse("MISSING110Y1")).rejects.toThrow(
      "Failed to fetch course with name MISSING110Y1"
    )
  })
})
