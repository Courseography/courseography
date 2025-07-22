import React from "react"
import { render, screen, cleanup } from "@testing-library/react"
import { NavBar } from "../NavBar.js.jsx"
import userEvent from "@testing-library/user-event"

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
    await screen.findByLabelText("Export")
  })

  it("renders export button on the grid page", async () => {
    render(<NavBar selected_page="grid" />)
    await screen.findByLabelText("Export")
  })

  it("does not render export button on the generate page", () => {
    render(<NavBar selected_page="generate" />)
    expect(screen.queryByLabelText("Export")).toBeNull()
  })
})

describe("test export button tooltip", () => {
  beforeEach(() => cleanup())

  it("tooltip appears on hover", async () => {
    // mock ResizeObserver to avoid ReferenceError when tooltip rendered
    global.ResizeObserver = class {
      observe() {}
      unobserve() {}
      disconnect() {}
    }

    render(<NavBar selected_page="graph" />)
    const exportButton = screen.getByLabelText("Export")
    await userEvent.hover(exportButton)
    expect(screen.queryByText("Export")).not.toBeNull()
  })

  it("tooltip does not appear if not hovering", () => {
    render(<NavBar selected_page="graph" />)
    expect(screen.queryByText("Export")).toBeNull()
  })
})
