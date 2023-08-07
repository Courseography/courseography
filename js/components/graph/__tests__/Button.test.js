import React from "react"
import renderer from "react-test-renderer"
import Button from "../Button"

describe("Button", () => {
  it("should match snapshot", () => {
    const buttonProps = {
      divId: "reset-button",
      text: "Reset",
      disabled: true,
    }
    const tree = renderer.create(<Button {...buttonProps} />).toJSON()
    expect(tree).toMatchSnapshot()
  })
})
