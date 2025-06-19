import React from "react"
import { render, screen, cleanup } from "@testing-library/react"
import { NavBar } from "../NavBar.js.jsx"

describe("test that navbar logo and items render correctly", () => {
  beforeEach(() => cleanup())

  it("displays Courseography logo and nav items", async () => {
    render(<NavBar selected_page="graph" />)
    await screen.findAllByText("Graph")
    await screen.findAllByText("Grid")
    await screen.findAllByText("Generate (beta)")
    await screen.findAllByText("About")
  })
})

describe("test conditional rendering of the export button", () => {
  beforeEach(() => cleanup())

  it("renders export button on the graph page", async () => {
    render(<NavBar selected_page="graph" />)
    await screen.findByAltText("Export")
  })

  it("renders export button on the grid page", async () => {
    render(<NavBar selected_page="grid" />)
    await screen.findByAltText("Export")
  })

  it("does not render export button on the generate page", () => {
    render(<NavBar selected_page="generate" />)
    expect(screen.queryByAltText("Export")).toBeNull()
  })
})
