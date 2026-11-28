import React from "react"
import { render, screen, cleanup } from "@testing-library/react"
import { ExportModal } from "../export"

describe("test that export modal displays correct content for graph and grid pages", () => {
  beforeEach(() => {
    cleanup()
    // Opening the modal requests the exported image with jQuery's $.ajax
    global.$ = { ajax: jest.fn(({ success }) => success("aW1hZ2U=")) }
  })

  afterEach(() => {
    delete global.$
  })

  it("test that modal displays correct content for graph page", async () => {
    render(<ExportModal page="graph" open={true}></ExportModal>)
    await screen.findByText("Export")
    await screen.findByText("Download PDF")
    expect(screen.queryByText("Download timetable as ICS")).toBeNull()
    expect(screen.queryByText("Switch Sessions")).toBeNull()
  })

  it("test that modal displays correct content for grid page", async () => {
    render(<ExportModal page="grid" session="fall" open={true}></ExportModal>)
    await screen.findByText("Export")
    await screen.findByText("Download PDF")
    await screen.findByText("Download timetable as ICS")
    await screen.findByText("Switch Sessions")
  })

  it("test that modal doesn't display its contents when it is not set to open", async () => {
    render(<ExportModal page="grid" open={false}></ExportModal>)
    expect(screen.queryByText("Export")).toBeNull()
  })

  describe("fetching the exported image", () => {
    it("requests and shows the graph image when the modal is opened", async () => {
      render(<ExportModal page="graph" open={true} />)
      expect(global.$.ajax).toHaveBeenCalledTimes(1)
      expect(global.$.ajax.mock.calls[0][0].url).toBe("/image")
      const image = await screen.findByRole("img")
      expect(image.getAttribute("src")).toBe("data:image/png;base64,aW1hZ2U=")
    })

    it("requests the timetable image for the given session when the modal is opened", async () => {
      render(<ExportModal page="grid" session="fall" open={true} />)
      expect(global.$.ajax).toHaveBeenCalledTimes(1)
      expect(global.$.ajax.mock.calls[0][0].url).toBe("/timetable-image")
      expect(global.$.ajax.mock.calls[0][0].data.session).toBe("Fall")
      const image = await screen.findByRole("img")
      expect(image.getAttribute("src")).toBe("data:image/png;base64,aW1hZ2U=")
    })

    it("does not request an image while the modal is closed", () => {
      render(<ExportModal page="graph" open={false} />)
      expect(global.$.ajax).not.toHaveBeenCalled()
    })

    it("requests a new image each time the modal is reopened", () => {
      const { rerender } = render(<ExportModal page="graph" open={true} />)
      rerender(<ExportModal page="graph" open={false} />)
      rerender(<ExportModal page="graph" open={true} />)
      expect(global.$.ajax).toHaveBeenCalledTimes(2)
    })
  })

  describe("when the image request fails", () => {
    it("shows an error message instead of the graph image", async () => {
      global.$.ajax = jest.fn(({ error }) => error())
      render(<ExportModal page="graph" open={true} />)
      const alert = await screen.findByRole("alert")
      expect(alert.textContent).toBe(
        "The graph image could not be generated. Please try again."
      )
      expect(screen.queryByRole("img")).toBeNull()
    })

    it("shows an error message instead of the timetable image", async () => {
      global.$.ajax = jest.fn(({ error }) => error())
      render(<ExportModal page="grid" session="fall" open={true} />)
      const alert = await screen.findByRole("alert")
      expect(alert.textContent).toBe(
        "The timetable image could not be generated. Please try again."
      )
      expect(screen.queryByRole("img")).toBeNull()
    })

    it("replaces a previously loaded image with the error message", async () => {
      const { rerender } = render(<ExportModal page="graph" open={true} />)
      await screen.findByRole("img")

      global.$.ajax = jest.fn(({ error }) => error())
      rerender(<ExportModal page="graph" open={false} />)
      rerender(<ExportModal page="graph" open={true} />)

      await screen.findByRole("alert")
      expect(screen.queryByRole("img")).toBeNull()
    })

    it("clears the error message once an image loads successfully", async () => {
      global.$.ajax = jest.fn(({ error }) => error())
      const { rerender } = render(<ExportModal page="graph" open={true} />)
      await screen.findByRole("alert")

      global.$.ajax = jest.fn(({ success }) => success("aW1hZ2U="))
      rerender(<ExportModal page="graph" open={false} />)
      rerender(<ExportModal page="graph" open={true} />)

      await screen.findByRole("img")
      expect(screen.queryByRole("alert")).toBeNull()
    })
  })
})
