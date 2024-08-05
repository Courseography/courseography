import React from "react"
import Container from "../Container"
import { render, act } from "@testing-library/react"

export default class TestContainer {
  /**
   * For async construction of the TestContainer
   * @return {Promise<Container>}
   */
  static async build() {
    const containerProps = {
      start_blank: false,
      edit: false,
    }

    // `act` needed to ensure async tasks are performed before we make any assertions
    const container = await act(async () => render(<Container {...containerProps} />))

    // Need to wait for lifecycle hooks
    const flushPromises = () => new Promise(setImmediate)
    await flushPromises()

    return container
  }
}
