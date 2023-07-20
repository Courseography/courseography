import React from "react"
import ShallowRenderer from "react-shallow-renderer"

import Button from "../Button"

describe("Button", () => {
  it("should match shallow snapshot", () => {
    const buttonProps = {
      divId: "reset-button",
      text: "Reset",
      disabled: true,
    }
    const renderer = new ShallowRenderer()
    renderer.render(<Button {...buttonProps} />)
    const button = renderer.getRenderOutput()
    expect(button).toMatchSnapshot()
  })
})
