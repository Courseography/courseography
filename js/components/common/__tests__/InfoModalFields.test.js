import React from "react"
import { render, screen, cleanup } from "@testing-library/react"
import { Description } from "../react_modal.js.jsx"

describe("Displays course information fields iff they are non-empty.", () => {
  beforeEach(() => cleanup())

  it("displays desc/prereq/coreq/excl/dist/breadth information if available", async () => {
    const courseInfo = {
      course: {
        name: "ABC102H1",
        description: "test description",
        prereqString: ["ABC101H1"],
        coreqs: ["DEF101H1"],
        exclusions: ["GHI101H1"],
        distribution: "Sciences",
        breadth: "The Physical and Mathematical Universes (5)",
      },
    }

    render(<Description course={courseInfo.course} sessions={{}} />)
    await screen.findByText("test description") // check that description shows
    await screen.findByText("ABC101H1") // check that prereqs show
    await screen.findByText("DEF101H1") // check that coreqs show
    await screen.findByText("GHI101H1") // check that exclusions show
    await screen.findByText("Sciences") // check that distribution shows
    await screen.findByText("The Physical and Mathematical Universes (5)") // check that breadth shows
  })

  it("do not display desc/prereq/coreq/excl/dist/breadth information if unavailable", async () => {
    const courseInfo = {
      course: {
        name: "ABC102H1",
        description: "",
        prereqString: [],
        coreqs: [],
        exclusions: [],
        distribution: "",
        breadth: "",
      },
    }

    render(<Description course={courseInfo.course} sessions={{}} />)
    expect(screen.queryByText("Prerequisite:")).toBeNull() // check prereq label does not render
    expect(screen.queryByText("Corequisite:")).toBeNull() // check coreq label does not render
    expect(screen.queryByText("Exclusion:")).toBeNull() // check exclusion label does not render
    expect(screen.queryByText("Distribution Requirement Status:")).toBeNull() // check dist. label does not render
    expect(screen.queryByText("Breadth Requirement:")).toBeNull() // check breadth label does not render
  })
})
