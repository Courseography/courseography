import React from "react";
import Container from "../Container";
import { render } from "@testing-library/react";

export default class TestContainer {
  /**
   * For async construction of the TestContainer
   * @return {Container}
   */
  static async build() {
    const containerProps = {
      start_blank: false,
      edit: false
    };

    const container = render(<Container {...containerProps} />);

    // Need to wait for lifecycle hooks
    const flushPromises = () => new Promise(setImmediate);
    await flushPromises();

    return container;
  }
}
