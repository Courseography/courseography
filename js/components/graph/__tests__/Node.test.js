import React from "react";
import { mount } from "enzyme";
import graphData from "../__mocks__/estonianData";
import Graph from "../Graph";
import Node from "../Node";

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

describe("Unselected Node", () => {

    it('should be red when hovered over', () => {
        // hover over CSC
    });

    it('should have a solid border if you have the prerequisites', () => {
        // EST100: solid border
        // EST101: dotted border

        // select EST100

        // check EST101 (solid), check EST102 (still dotted)

        // deselect EST100 
        // recheck EST100, 101
    });
});

describe('Selected Course Node', () => {
    it('should black when pre-reqs are met', () => {
        // Select EST100
        
        // check border

        // deselect EST100
    });
    it('should be red with unmet pre-reqs', () => {
        // select EST101

        // expect EST100
        // deselect EST101
    });

    it('when hovered, should highlight all unmet pre-reqs', () => {
        // select EST300

        // expect EST201, EST200, EST101, EST1000 to be all red

        // deselect EST300
    });
})
