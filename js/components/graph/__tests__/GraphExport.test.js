import React from "react"
import { render, screen, cleanup, fireEvent } from "@testing-library/react"
import Container from "../Container.js"

describe("test export modal functionality on graph page", () => {
  beforeEach(() => cleanup())

  it("test that export modal for graph pops up when export button clicked", async () => {
    render(<Container></Container>)
    const exportButton = screen.getByLabelText("Export")
    // check that export modal is not visible before button clicked, but is visible after
    expect(screen.queryByText("Export")).toBeNull()
    fireEvent.click(exportButton)
    await screen.findByText("Export")
  })
})
