import React from "react";
import { shallow } from "enzyme";
import { cleanup, fireEvent } from "react-testing-library";
import setupGraph from "./setupGraph";

afterEach(cleanup);

import Button from "../Button";

describe("Button", () => {
  it("should match shallow snapshot", () => {
    const buttonProps = {
      divId: "reset-button",
      text: "Reset",
      disabled: true
    };
    const component = shallow(<Button {...buttonProps} />);
    expect(component).toMatchSnapshot();
  });

  it("all buttons except the zoom-in and out should be disabled on load", async () => {
    const graph = await setupGraph();
    const zoomInBtn = graph.getByText("+");
    const zoomOutBtn = graph.getByText("\u2014"); // mdash encoding
    const upBtn = graph.getByText("↑");
    const downBtn = graph.getByText("↓");
    const leftBtn = graph.getByText("→");
    const rightBtn = graph.getByText("←");
    const resetBtn = graph.getByText("Reset");
    expect(zoomInBtn.hasAttribute("disabled")).toBe(false);
    expect(zoomOutBtn.hasAttribute("disabled")).toBe(false);
    expect(upBtn.hasAttribute("disabled")).toBe(true);
    expect(rightBtn.hasAttribute("disabled")).toBe(true);
    expect(leftBtn.hasAttribute("disabled")).toBe(true);
    expect(downBtn.hasAttribute("disabled")).toBe(true);
    expect(resetBtn.hasAttribute("disabled")).toBe(true);
  });

  // it('reset button should be disabled when the graph is in the default zoom', async () => {
  //   const graph = await setupGraph();
  //   const zoomInBtn = graph.getByText("+");
  //   const zoomOutBtn = graph.getByText("\u2014"); // mdash encoding
  //   const resetBtn = graph.getByText("Reset");
  //   expect(resetBtn.hasAttribute("disabled")).toBe(true);

  //   fireEvent.click(zoomInBtn);
  //   expect(resetBtn.hasAttribute("disabled")).toBe(false);

  //   fireEvent.click(zoomOutBtn);
  //   expect(resetBtn.hasAttribute("disabled")).toBe(true);
  // });

  // it("zooming increases the size of the nodes and zooming out decreases the size of the nodes", async () => {
  //   /**
  //    * @returns {string} - the viewbox attribute of the svg
  //    */
  //   function getSVGViewBox() {
  //     return document.getElementsByTagName("svg")[0].getAttribute("viewBox");
  //   }

  //   const graph = await setupGraph();
  //   const zoomInBtn = graph.getByText("+");
  //   const zoomOutBtn = graph.getByText("\u2014"); // mdash encoding
  //   const resetBtn = graph.getByText("Reset");
  //   const initial = getSVGViewBox();

  //   fireEvent.click(zoomOutBtn);
  //   const afterClick = getSVGViewBox();

  //   expect(initial).not.toBe(afterClick);
  // });
});
