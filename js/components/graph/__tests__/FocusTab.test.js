import { fireEvent, waitFor } from "@testing-library/react"
import TestContainer from "./TestContainer"

describe("FocusTab", () => {
  it("When focuses are clicked, focuses should be active", async () => {
    const container = await TestContainer.build()
    fireEvent.click(container.getByRole("button", { name: /FOCUSES ⪢/i }))
    const focusDiv = container
      .getByRole("button", { name: /scientific computing/i })
      .closest("div")
    expect(focusDiv.classList.contains("active-focus")).toBe(false)
    fireEvent.click(container.getByRole("button", { name: /scientific computing/i }))
    expect(focusDiv.classList.contains("active-focus")).toBe(true)
  })

  it("When focuses are clicked, the graph should be highlighted", async () => {
    const container = await TestContainer.build()
    expect(
      container.getByTestId("react-graph").classList.contains("highlight-nodes")
    ).toBe(false)
    fireEvent.click(container.getByRole("button", { name: /FOCUSES ⪢/i }))
    fireEvent.click(container.getByRole("button", { name: /scientific computing/i }))
    await waitFor(() =>
      expect(
        container.getByTestId("react-graph").classList.contains("highlight-nodes")
      ).toBe(true)
    )
  })

  it("When focuses are clicked, an information button should appear", async () => {
    const container = await TestContainer.build()
    expect(container.queryByRole("button", { name: "i" })).toBeNull
    fireEvent.click(container.getByRole("button", { name: /FOCUSES ⪢/i }))
    fireEvent.click(container.getByRole("button", { name: /scientific computing/i }))
    expect(container.queryByRole("button", { name: /Focus Description/i }))
      .toBeInTheDocument
  })
  it("When focuses are clicked twice, the information button should appear and disappear", async () => {
    const container = await TestContainer.build()
    // information button should not be in the doucment
    expect(container.queryByRole("button", { name: /Focus Description/ })).toBeNull
    // click on the focus button
    fireEvent.click(container.getByRole("button", { name: /FOCUSES ⪢/i }))
    // click on a focus
    fireEvent.click(container.getByRole("button", { name: /scientific computing/i }))
    // information button should appear
    expect(container.queryByRole("button", { name: /Focus Description/i }))
      .toBeInTheDocument
    // click on the focus again
    fireEvent.click(container.getByRole("button", { name: /scientific computing/i }))
    // information button should disappear
    expect(container.queryByRole("button", { name: /Focus Description/i })).not
      .toBeInTheDocument
  })
  it("When focuses are clicked twice, the graph should be highlighted and then unhighlighted", async () => {
    const container = await TestContainer.build()
    expect(
      // test id is found in Graph.js which gives reference to the main graph
      container.getByTestId("react-graph").classList.contains("highlight-nodes")
    ).toBe(false)
    fireEvent.click(container.getByRole("button", { name: /FOCUSES ⪢/i }))
    fireEvent.click(container.getByRole("button", { name: /scientific computing/i }))
    await waitFor(() =>
      expect(
        // check the graph contains class name "highlighted-nodes"
        container.getByTestId("react-graph").classList.contains("highlight-nodes")
      ).toBe(true)
    )
    // click again to remove the highlights
    fireEvent.click(container.getByRole("button", { name: /scientific computing/i }))
    await waitFor(() => {
      expect(
        container.getByTestId("react-graph").classList.contains("highlight-nodes")
      ).toBe(false)
    })
  })
})
