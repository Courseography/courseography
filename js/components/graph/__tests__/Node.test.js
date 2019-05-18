import React from "react";
import { shallow } from "enzyme";
import { cleanup } from "react-testing-library";
import Node from "../Node";
import setupGraph from "./setupGraph";

afterEach(cleanup);

describe("Hybrid Node", () => {
});

describe("Course Node", () => {
  it("should match shallow snapshot", () => {
    const courseProps = {
      JSON: {
        fill: "#5dd5b8",
        graph: 1,
        height: 30,
        id_: "csc108",
        pos: [594.294128, 38.09390799999999],
        stroke: "",
        text: [
          {
            align: "begin",
            fill: "",
            graph: 1,
            pos: [596.555968, 59.379107999999995],
            rId: "text102",
            text: "CSC108",
            type_: "Node",
            width: 65
          }
        ]
      },
      childs: ["csc148"],
      className: "node",
      highlighted: false,
      hybrid: false,
      inEdges: [],
      onClick: jest.fn(),
      onMouseEnter: jest.fn(),
      onMouseLeave: jest.fn(),
      outEdges: ["p6"],
      parents: [],
      svg: {}
    };

    const wrapper = shallow(<Node {...courseProps} />);
    expect(wrapper).toMatchSnapshot();
  });

  it("should render Node component properly with proper course code", async () => {
    const graph = await setupGraph();
    const courseTextNode = graph.getByText("AAA100");
    expect(courseTextNode.parentNode.id.toUpperCase()).toBe(
      courseTextNode.innerHTML
    );
  });
});
