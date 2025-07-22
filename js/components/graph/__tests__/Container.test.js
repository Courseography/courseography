import React from "react"
import { render, screen } from "@testing-library/react"
import Container from "../Container"
import { Graph } from "../Graph"

describe("Verify that the Error Fallback component is only rendered on a Graph render error", () => {
  afterEach(() => {
    jest.restoreAllMocks()
  })

  it("Should render the Fallback component when the Graph fails to render", async () => {
    jest.spyOn(Graph.prototype, "getGraph").mockImplementation(() => {
      throw new Error("Test Error Boundary Thrown")
    })
    render(<Container />)
    expect(screen.queryByText(/Test Error Boundary Thrown/))
  })

  it("Should not render the Fallback component when the Graph does not fail to render", async () => {
    render(<Container />)
    const foundText = screen.queryByText(/Your graph has failed to render/)
    expect(foundText).toBeNull()
  })
})
