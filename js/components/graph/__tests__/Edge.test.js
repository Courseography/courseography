import React from "react";
import { shallow } from "enzyme";
import Edge from "../Edge";
import setupGraph from "./setupGraph";
import { cleanup, fireEvent } from "react-testing-library";

afterEach(cleanup);

describe("Edge", () => {
  function getPath(graph, name) {
    // the user only cares about the location of the paths
    const edgeLocation = {
      "h101-201": "M626.794148,68.11810799999999 626.794148,80.89120799999999 ",
      "201-and":
        "M659.307448,103.86880799999999 765.952148,103.86880799999999 765.952148,131.888008 ",
      "102-and": "M515,70 515,144 748,144 ",
      "and-303": "M765.952148,155 765.952148,171 ",
      "102-or": "M515,70 540,90 ",
      "201-or": "M592,100 570,100 ",
      "or-202": "M550,100 550,173 "
    };

    if (edgeLocation[name] === undefined) {
      throw new Error(`Path "${name}" not found!`);
    }

    const paths = graph.container.getElementsByTagName("path");
    for (let i = 0; i < paths.length; i += 1) {
      if (paths[i].getAttribute("d") === edgeLocation[name]) {
        return paths[i];
      }
    }
  }
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

  it("should be initialized with the class 'inactive'", async () => {
    await setupGraph();
    const paths = document.getElementsByTagName("path");
    for (let i = 0; i < paths.length; i += 1) {
      expect(paths[i].classList.contains("inactive")).toBe(true);
      expect(paths[i].classList.contains("path")).toBe(true);
    }
  });

  describe("Clicking course nodes", () => {
    it("unselected source and destination should have an 'inactive' Edge", async () => {
      const graph = await setupGraph();
      const h101_201 = getPath(graph, "h101-201");
      expect(h101_201.classList.contains("inactive")).toBe(true);
    });
    it("with selected source and unselected destination should have a 'takeable' Edge", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const h101_201 = getPath(graph, "h101-201");

      fireEvent.click(aaa101);
      expect(h101_201.classList.contains("takeable")).toBe(true);
    });

    it("with selected source with unselected selected destination should still have an 'inactive' Edge", async () => {
      const graph = await setupGraph();
      const aaa201 = graph.getByText("AAA201").parentNode;
      const h101_201 = getPath(graph, "h101-201");
      expect(h101_201.classList.contains("inactive")).toBe(true);

      fireEvent.click(aaa201);
      expect(h101_201.classList.contains("inactive")).toBe(true);
    });

    it("with selected source and destination should have an 'active' Edge", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      const h101_201 = getPath(graph, "h101-201");
      fireEvent.click(aaa101);
      fireEvent.click(aaa201);

      expect(h101_201.classList.contains("active")).toBe(true);
    });
  });

  describe("hovering behaviour", async () => {
    it("hovering over the source does nothing, regardless of the state of the source and destination", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      const h101_201 = getPath(graph, "h101-201");

      expect(h101_201.classList.contains("inactive")).toBe(true);
      fireEvent.mouseOver(aaa101);
      expect(h101_201.classList.contains("inactive")).toBe(true);
      fireEvent.mouseOut(aaa101);

      fireEvent.click(aaa101); // select
      expect(h101_201.classList.contains("takeable")).toBe(true);
      fireEvent.mouseOver(aaa101);
      expect(h101_201.classList.contains("takeable")).toBe(true);
      fireEvent.mouseOut(aaa101);

      fireEvent.click(aaa201); // select
      expect(h101_201.classList.contains("active")).toBe(true);
      fireEvent.mouseOver(aaa101);
      expect(h101_201.classList.contains("active")).toBe(true);
      fireEvent.mouseOut(aaa101);

      fireEvent.click(aaa101); // deselect
      expect(h101_201.classList.contains("inactive")).toBe(true);
      fireEvent.mouseOver(aaa101);
      expect(h101_201.classList.contains("inactive")).toBe(true);
      fireEvent.mouseOut(aaa101);
    });

    it("selected source and unselected destination, hovering over the destination will have the Edge remain 'takeable'", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      const h101_201 = getPath(graph, "h101-201");
      fireEvent.click(aaa101);
      expect(h101_201.classList.contains("takeable")).toBe(true);
      fireEvent.mouseOver(aaa201);
      expect(h101_201.classList.contains("takeable")).toBe(true);
    });

    it("selected source and selected destination, hovering over the destination will have the Edge remain 'active'", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      const h101_201 = getPath(graph, "h101-201");
      fireEvent.click(aaa101);
      fireEvent.click(aaa201);
      expect(h101_201.classList.contains("active")).toBe(true);
      fireEvent.mouseOver(aaa201);
      expect(h101_201.classList.contains("active")).toBe(true);
    });
    // the graph creates a new <path> instead of modifying the existing graph
    // when the hovering is over, the original path is put back in the exact same order
    it("hovering over a destination && unselected source => edge should be 'missing'", async () => {
      const graph = await setupGraph();
      const aaa303 = graph.getByText("AAA303").parentNode;
      // initialize node
      fireEvent.mouseOver(aaa303);
      fireEvent.mouseOut(aaa303);

      fireEvent.mouseOver(aaa303);

      const h101_201 = getPath(graph, "h101-201");
      const n201_and = getPath(graph, "201-and");
      const n102_and = getPath(graph, "102-and");
      const and_303 = getPath(graph, "and-303");

      expect(h101_201.classList.contains("missing")).toBe(true);
      expect(n201_and.classList.contains("missing")).toBe(true);
      expect(n102_and.classList.contains("missing")).toBe(true);
      expect(and_303.classList.contains("missing")).toBe(true);
      fireEvent.mouseOut(aaa303);

      fireEvent.click(aaa303); // selected node with missing pre-reqs
      fireEvent.mouseOver(aaa303);
      expect(h101_201.classList.contains("missing")).toBe(true);
      expect(n201_and.classList.contains("missing")).toBe(true);
      expect(n102_and.classList.contains("missing")).toBe(true);
      expect(and_303.classList.contains("missing")).toBe(true);
    });
  });
});
