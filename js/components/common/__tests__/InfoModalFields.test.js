import React from "react"
import { render, screen, cleanup } from "@testing-library/react"
import { Description } from "../react_modal.js.jsx"

describe("Displays course information fields iff they are non-empty.", () => {
  beforeEach(() => cleanup())

  it("displays desc/prereq/coreq/excl/dist/breadth information if available", async () => {
    const courseInfo = {
      name: "ABC102H1",
      description: "test description",
      prereqString: ["ABC101H1"],
      coreqs: ["DEF101H1"],
      exclusions: ["GHI101H1"],
      distribution: "Sciences",
      breadth: "The Physical and Mathematical Universes (5)",
    }

    render(<Description course={courseInfo} sessions={{}} />)
    await screen.findByText("test description")
    await screen.findByText("ABC101H1")
    await screen.findByText("DEF101H1")
    await screen.findByText("GHI101H1")
    await screen.findByText("Sciences")
    await screen.findByText("The Physical and Mathematical Universes (5)")
  })

  it("do not display desc/prereq/coreq/excl/dist/breadth information if unavailable", async () => {
    const courseInfo = {
      name: "ABC102H1",
      description: "",
      prereqString: [],
      coreqs: [],
      exclusions: [],
      distribution: "",
      breadth: "",
    }

    render(<Description course={courseInfo} sessions={{}} />)
    expect(screen.queryByText("Prerequisite:")).toBeNull()
    expect(screen.queryByText("Corequisite:")).toBeNull()
    expect(screen.queryByText("Exclusion:")).toBeNull()
    expect(screen.queryByText("Distribution Requirement Status:")).toBeNull()
    expect(screen.queryByText("Breadth Requirement:")).toBeNull()
  })
})
