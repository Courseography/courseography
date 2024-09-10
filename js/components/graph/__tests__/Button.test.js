import React from "react"
import Button from "../Button"
import { render, screen, act } from "@testing-library/react"

describe("Button", () => {
  it("should match snapshot", async () => {
    const buttonText = "Reset"
    const buttonProps = {
      divId: "reset-button",
      text: buttonText,
      disabled: true,
    }
    await act(async () => render(<Button {...buttonProps} />))
    const buttonElement = screen.getByText(buttonText)
    expect(buttonElement).toMatchSnapshot()
  })
})
