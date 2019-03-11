import React from "react";
import Graph from "../Graph";
import { shallow } from "enzyme";
import waitUntil from "async-wait-until";
import { render, cleanup } from "react-testing-library";
import Node from "../Node";

/**
 * @param {function} done - provided by Jest to indicate the completion of an async operation
 * @returns {Graph}
 */
async function setupGraph(done) {
  const graphProps = {
    edit: false,
    initialDrawMode: "draw-node",
    initialOnDraw: false,
    start_blank: false
  };

  const graph = render(<Graph {...graphProps} />);
  await waitUntil(() => graph.queryByText("AAA100") !== null);
  expect(graph.queryByText("AAA100")).toBeTruthy();
  done();
  return graph;
}

afterEach(cleanup);

describe("Hybrid Node", () => {
  it("node", () => {
    const nodeProps = {
      JSON: {
        fill: "#888888",
        graph: 1,
        height: 24,
        id_: "h46",
        pos: [18.168848, 471.586698],
        stroke: "",
        text: [
          {
            align: "begin",
            fill: "",
            graph: 1,
            pos: [20.753048200000002, 481.389408],
            rId: "text454",
            text: "CSC318/418/",
            type_: "Hybrid",
            width: 65.207497
          },
          {
            align: "begin",
            fill: "",
            graph: 1,
            pos: [32.698848, 493.640408],
            rId: "text456",
            text: "301/384",
            length: 2,
            type_: "Hybrid",
            width: 65.207497
          }
        ],
        type_: "Hybrid",
        width: 65.207497
      },
      childs: ["csc404"],
      className: "hybrid",
      hybrid: true,
      inEdges: [],
      logicalType: "AND",
      outEdges: ["p32"],
      parents: [],
      svg: {
        onKeyDown: jest.fn()
      }
    };
    const wrapper = shallow(<Node {...nodeProps} />);
    expect(wrapper).toMatchSnapshot();
  });


  it("shouldn't do anything when you hover or click it", () => { });
  it("should become selected when its pre-req parent is satisfied", () => { });
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

  it("should render Node component properly with proper course code", async done => {
    const graph = await setupGraph(done);
    const courseTextNode = graph.getByText("AAA100");
    expect(courseTextNode.parentNode.id.toUpperCase()).toBe(
      courseTextNode.innerHTML
    );
  });

  it("should create an info box when hovering over the course", () => {});

  it.skip("should have a solid border if you have the prerequisites", () => {});


});

describe("Unselected Course Node", () => {
  it ('should be red when hovered over', () => {});

  it.skip("should have a solid border if you have the prerequisites", () => {});
});

describe("Selected Course Node", () => {
  it("should black when pre-reqs are met", () => {
    // Select EST100
    // check border
    // deselect EST100
  });

  it("should be red with unmet pre-reqs", () => {
    // select EST101
    // expect EST100
    // deselect EST101

    // inactive, not takeable
  });

  it("when hovered, should highlight all unmet pre-reqs", () => {
    // select EST300
    // expect EST201, EST200, EST101, EST1000 to be all red
    // deselect EST300
  });
});
