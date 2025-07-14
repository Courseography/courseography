import React from "react"
import { render, screen, cleanup } from "@testing-library/react"
import { ExportModal } from "../export.js.jsx"

describe("test that export modal displays correct content for graph and grid pages", () => {
  beforeEach(() => cleanup())

  it("test that modal displays correct content for graph page", async () => {
    render(<ExportModal page="graph" open={true}></ExportModal>)
    await screen.findByText("Export")
    await screen.findByText("Download PDF")
    expect(screen.queryByText("Download timetable as ICS")).toBeNull()
    expect(screen.queryByText("Switch Sessions")).toBeNull()
  })

  it("test that modal displays correct content for grid page", async () => {
    render(<ExportModal page="grid" open={true}></ExportModal>)
    await screen.findByText("Export")
    await screen.findByText("Download PDF")
    await screen.findByText("Download timetable as ICS")
    await screen.findByText("Switch Sessions")
  })

  it("test that modal doesn't display its contents when it is not set to open", async () => {
    render(<ExportModal page="grid" open={false}></ExportModal>)
    expect(screen.queryByText("Export")).toBeNull()
  })
})
