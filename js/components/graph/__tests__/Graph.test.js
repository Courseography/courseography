import React from "react";
import { mount } from "enzyme";
import graphData from "../__mocks__/graphData";
import Graph from "../Graph";

let graph;

beforeAll(() => {
  const graphProps = {
    ...graphData,
    edit: false,
    initialDrawMode: "draw-node",
    initialOnDraw: false,
    start_blank: false
  };
  graph = mount(<Graph {...graphProps} />);
});

describe("Graph Node", () => {
  it.skip("Hovering over a course node should make a info box appear", () => {
    const csc165 = graph.find("#csc165240");

    csc165.simulate("mouseout");

    expect(graph.exists("g#infoBox")).toBe(false);
    expect(csc165.hasClass("takeable")).toBe(true);

    csc165.simulate("mouseover");
    expect(csc165.hasClass("missing")).toBe(true);

    expect(graph.exists("g#infoBox")).toBe(true);
    expect(graph.exists("g.tooltip-group")).toBe(true);

    csc165.simulate("mouseout");
    expect(csc165.hasClass("takeable"));
    expect(graph.exists("g#infoBox")).toBe(false);
  });
  it.skip("clicking a course node: triggers changes in the path and children nodes (including hybrid nodes)", () => {
    // before
    const hybridCSC165 = graph.find("g#h68"); // hardcoded
    const csc165 = graph.find("#csc165240");
    const csc165To236 = graph.find(
      'path[d="M497.651848,69.09890799999998 497.651848,130.885308 "]'
    ); // extremely hard-coded

    expect(csc165.hasClass("takeable")).toBe(true);
    expect(csc165To236.hasClass("inactive")).toBe(true);
    expect(hybridCSC165.hasClass("inactive")).toBe(true);

    csc165.simulate("click");

    // after
    expect(csc165.hasClass("active")).toBe(true);
    expect(csc165To236.hasClass("takeable")).toBe(true);
    expect(hybridCSC165.hasClass("active")).toBe(true);

    // cleanup
    csc165.simulate("click");
    expect(csc165.hasClass("takeable")).toBe(true);
    expect(csc165To236.hasClass("inactive")).toBe(true);
    expect(hybridCSC165.hasClass("inactive")).toBe(true);
  });

  it("Pressing a course node should increase the FCE count by 0.5 if it's a half-year course", () => {
    
  });
  it("Pressing a course node should increase the FCE count by 1.0 if it's a full-year course", () => {

  });
});
