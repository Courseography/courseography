import React from "react";
import { mount } from "enzyme";
import Graph from "../Graph";

let graph;

beforeAll(() => {
    const graphProps = {
        edit: false,
        initialDrawMode: "draw-node",
        initialOnDraw: false,
        start_blank: false
    };

    // TODO: mount has the ajax call
    graph = mount(<Graph {...graphProps} />);
});

describe("Graph Node", () => {
    it("Hovering over a course node should make a info box appear", () => {
        graph.update();
        // graph now has data
    });
    it("clicking a course node: triggers changes in the path and children nodes (including hybrid nodes)", () => {

    });

    it("Pressing a course node should increase the FCE count by 0.5 if it's a half-year course", () => {

    });
    it("Pressing a course node should increase the FCE count by 1.0 if it's a full-year course", () => {

    });
});
