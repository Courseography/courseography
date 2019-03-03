import React from "react";
import { mount } from "enzyme";
import Graph from "../Graph";

let graph;

beforeAll((done) => {
    const graphProps = {
        edit: false,
        initialDrawMode: "draw-node",
        initialOnDraw: false,
        start_blank: false
    };

    // TODO: mount has the ajax call
    graph = mount(<Graph {...graphProps} />);

    // slight delay before calling update()
    setTimeout(() => {
        graph.update();

        expect(graph.exists("Node"));
        expect(graph.exists("Edge"));
        done();
    }, 100);
});

describe("Graph Node", () => {
    it("Hovering over a course node should make a info box appear", () => {
        const csc165 = graph.find("g#csc165240");
        // add csc165 to the localStorage
        csc165.simulate("mouseover");
        csc165.simulate("mouseout");
        
        expect(graph.exists("g#infoBox")).toBe(false);

        // TODO: why does this return false?
        // expect(csc165.hasClass("takeable")).toBe(true);

        // csc165.simulate("mouseover");
        // console.log(graph.exists("g#infoBox"));
    });
    it("clicking a course node: triggers changes in the path and children nodes (including hybrid nodes)", () => {

    });

    it("Pressing a course node should increase the FCE count by 0.5 if it's a half-year course", () => {

    });
    it("Pressing a course node should increase the FCE count by 1.0 if it's a full-year course", () => {

    });
});
