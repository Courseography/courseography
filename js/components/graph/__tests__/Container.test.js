import React from "react"
import { render } from "@testing-library/react"
import Container from "../Container"

jest.mock("../Graph", () => ({
  __esModule: true,
  default: () => {
    throw new Error("Test Fallback Error Boundary")
  },
}))

describe("Verify that the Error Fallback component is only rendered on a Graph render error", () => {
  it("Should render the Fallback component when the Graph fails to render", () => {
    const result = render(<Container />)
    const foundFallback = result.container.querySelector("#graph-fallback")
    expect(foundFallback).not.toBeNull()
  })
})
