import React from "react"
//import ShallowRenderer from "react-shallow-renderer"
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
    //    const renderer = new ShallowRenderer()
    //    renderer.render(<Button {...buttonProps} />)
    //    const button = renderer.getRenderOutput()
    //    expect(button).toMatchSnapshot()
  })
})
