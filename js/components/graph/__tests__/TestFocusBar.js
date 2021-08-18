import React from "react"
import FocusBar from "../FocusBar"
import { render } from "@testing-library/react"

export default class TestFocusBar {
  /**
   * For async construction of the TestFocusBar
   * @return {TestFocusBar}
   */
  static async build() {
    const focusBarProps = {
      currFocus: null,
      focusBarEnabled: true,
    }

    const focusBar = render(<FocusBar {...focusBarProps} />)
    return focusBar
  }
}
