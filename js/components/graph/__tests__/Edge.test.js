import React from "react";
import { shallow } from "enzyme";
import TestGraph from "./TestGraph";
import { fireEvent } from "react-testing-library";

import Edge from "../Edge";

describe("Edge", () => {
  it("should match shallow snapshot", () => {
    const edgeProps = {
      className: "path",
      edgeID: "p1",
      points: [[497.651848, 69.09890799999998], [497.651848, 130.885308]],
      source: "csc165240",
      target: "csc236240",
      updateEdgeStatus: null,
      svg: {}
    };
    const component = shallow(<Edge {...edgeProps} />);
    expect(component).toMatchSnapshot();
  });

  describe("Clicking course nodes", () => {
    it("unselected source and destination => 'inactive' Edge", async () => {
      const graph = await TestGraph.build();
      const path_h101_201 = graph.getPath("h101-201");
      expect(path_h101_201.classList.contains("inactive")).toBe(true);
    });
    it("with selected source and unselected destination => 'takeable' Edge", async () => {
      const graph = await TestGraph.build();
      const aaa101 = graph.getNodeByText("AAA101");
      const path_101_201 = graph.getPath("h101-201");

      fireEvent.click(aaa101);
      expect(path_101_201.classList.contains("takeable")).toBe(true);
    });

    it("with unselected source with selected destination => 'inactive' Edge", async () => {
      const graph = await TestGraph.build();
      const aaa201 = graph.getNodeByText("AAA201");
      const path_101_201 = graph.getPath("h101-201");
      expect(path_101_201.classList.contains("inactive")).toBe(true);

      fireEvent.click(aaa201);
      expect(path_101_201.classList.contains("inactive")).toBe(true);
    });

    it("with selected source and destination => 'active' Edge", async () => {
      const graph = await TestGraph.build();
      const aaa101 = graph.getNodeByText("AAA101");
      const aaa201 = graph.getNodeByText("AAA201");
      const path_101_201 = graph.getPath("h101-201");
      fireEvent.click(aaa101);
      fireEvent.click(aaa201);

      expect(path_101_201.classList.contains("active")).toBe(true);
    });
  });

  describe("hovering behaviour", () => {
    it("hovering over the source does nothing, regardless of the state of the source and destination", async () => {
      const graph = await TestGraph.build();
      const aaa101 = graph.getNodeByText("AAA101");
      const aaa201 = graph.getNodeByText("AAA201");
      const path_101_201 = graph.getPath("h101-201");

      expect(path_101_201.classList.contains("inactive")).toBe(true);
      fireEvent.mouseOver(aaa101);
      expect(path_101_201.classList.contains("inactive")).toBe(true);
      fireEvent.mouseOut(aaa101);

      fireEvent.click(aaa101); // select
      expect(path_101_201.classList.contains("takeable")).toBe(true);
      fireEvent.mouseOver(aaa101);
      expect(path_101_201.classList.contains("takeable")).toBe(true);
      fireEvent.mouseOut(aaa101);

      fireEvent.click(aaa201); // select
      expect(path_101_201.classList.contains("active")).toBe(true);
      fireEvent.mouseOver(aaa101);
      expect(path_101_201.classList.contains("active")).toBe(true);
      fireEvent.mouseOut(aaa101);

      fireEvent.click(aaa101); // deselect
      expect(path_101_201.classList.contains("inactive")).toBe(true);
      fireEvent.mouseOver(aaa101);
      expect(path_101_201.classList.contains("inactive")).toBe(true);
      fireEvent.mouseOut(aaa101);
    });

    it("selected source and unselected destination and hovering over the destination => edge remains 'takeable'", async () => {
      const graph = await TestGraph.build();
      const aaa101 = graph.getNodeByText("AAA101");
      const aaa201 = graph.getNodeByText("AAA201");
      const path_101_201 = graph.getPath("h101-201");
      fireEvent.click(aaa101);
      expect(path_101_201.classList.contains("takeable")).toBe(true);
      fireEvent.mouseOver(aaa201);
      expect(path_101_201.classList.contains("takeable")).toBe(true);
    });

    it("hovering over a selected destination && unselected source => edge should be 'missing'", async () => {
      const graph = await TestGraph.build();
      const aaa303 = graph.getNodeByText("AAA303");
      const path_101_201 = graph.getPath("h101-201");
      const path_201_and = graph.getPath("201-and");
      const path_102_and = graph.getPath("102-and");
      const path_and_303 = graph.getPath("and-303");

      fireEvent.click(aaa303); // selected node with missing pre-reqs

      expect(path_101_201.classList.contains("inactive")).toBe(true);
      expect(path_201_and.classList.contains("inactive")).toBe(true);
      expect(path_102_and.classList.contains("inactive")).toBe(true);
      expect(path_and_303.classList.contains("inactive")).toBe(true);

      fireEvent.mouseOver(aaa303);
      expect(path_101_201.classList.contains("missing")).toBe(true);
      expect(path_201_and.classList.contains("missing")).toBe(true);
      expect(path_102_and.classList.contains("missing")).toBe(true);
      expect(path_and_303.classList.contains("missing")).toBe(true);
    });

    it("selected source and selected destination, hovering over the destination will have the Edge remain 'active'", async () => {
      const graph = await TestGraph.build();
      const aaa101 = graph.getNodeByText("AAA101");
      const aaa201 = graph.getNodeByText("AAA201");
      const path_101_201 = graph.getPath("h101-201");
      fireEvent.click(aaa101);
      fireEvent.click(aaa201);
      expect(path_101_201.classList.contains("active")).toBe(true);
      fireEvent.mouseOver(aaa201);
      expect(path_101_201.classList.contains("active")).toBe(true);
    });
  });
});
