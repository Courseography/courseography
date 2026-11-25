import React from "react"
import { render, screen, cleanup, fireEvent } from "@testing-library/react"
import Grid from "../grid"
import fetchMock from "fetch-mock"

describe("test export modal functionality on grid page", () => {
  beforeEach(() => {
    cleanup()
    fetchMock.restore()
    fetchMock.get("/courses", [])
    // Opening the export modal requests the timetable image with jQuery's $.ajax
    global.$ = { ajax: jest.fn() }
  })

  it("test that export modal for grid pops up when export button clicked", async () => {
    render(<Grid></Grid>)
    const exportButton = screen.getByLabelText("Export")
    // check that export modal is not visible before button clicked, but is visible after
    expect(screen.queryByText("Export")).toBeNull()
    fireEvent.click(exportButton)
    await screen.findByText("Export")
  })

  afterEach(() => {
    fetchMock.restore()
    delete global.$
  })
})
