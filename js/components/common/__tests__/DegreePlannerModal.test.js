import { screen, waitFor, fireEvent } from "@testing-library/react"
import TestContainer from "../../graph/__tests__/TestContainer.js"

describe("DegreeModal", () => {
  it("Clicking on the 'Create plan' button opens up the modal", async () => {
    await TestContainer.build()

    // Click on the 'Create plan' button inside the sidebar
    const createPlanButton = screen.getByText("Create plan")
    fireEvent.click(createPlanButton)

    // Wait for the degree modal to open
    await waitFor(() => {
      const modal = document.querySelector(".degree-planner-modal")
      return modal !== null
    })

    expect(screen.getByText("Courseography Degree Planner")).not.toBeNull()
  })

  it("The sidebar's selected courses appear inside the degree modal", async () => {
    const container = await TestContainer.build()

    // Click on the AAA100 course node
    const aaa100 = container.getByTestId("aaa100")
    fireEvent.click(aaa100)

    // Click on the 'Create plan' button inside the sidebar
    const createPlanButton = screen.getByText("Create plan")
    fireEvent.click(createPlanButton)

    // Wait for the degree modal to open
    await waitFor(() => {
      const modal = document.querySelector(".degree-planner-modal")
      return modal !== null
    })

    // Check that the selected course AAA100 also appears inside the degree modal
    const AAA100 = screen.getByText("AAA100", { selector: ".course-node" })
    expect(AAA100).toBeDefined()
  })
})
