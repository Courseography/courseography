import React from "react";
import Graph from "../Graph";
import { shallow } from "enzyme";
import waitUntil from "async-wait-until";
import { render, fireEvent } from "react-testing-library";
import Node from "../Node";

let graph;

beforeEach(async done => {
  const graphProps = {
    edit: false,
    initialDrawMode: "draw-node",
    initialOnDraw: false,
    start_blank: false
  };

  graph = render(<Graph {...graphProps} />);
  // wait until the graph's fetch() call is complete
  await waitUntil(() => graph.queryByText("CSC104") !== null);

  expect(graph.queryByText("CSC104")).toBeTruthy();
  done();
});

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

  it("should render Node component properly with proper course code", () => {
    const courseTextNode = graph.getByText("CSC104");
    // Check if svg container id is consistent with course code
    expect(courseTextNode.parentNode.id.toUpperCase()).toBe(courseTextNode.innerHTML);
    // TODO: Check if it has a sibling rect element?
  });

  it("should should create an info box when hovering over the course", () => {
    const courseNode = graph.getByText("CSC104").parentNode;

    // initially, nodes only have the class "node"
    // it gains other CSS classes with events
    fireEvent.mouseOut(courseNode);
    expect(courseNode.classList.contains("takeable")).toBe(true);
    fireEvent.mouseOver(courseNode);
    expect(courseNode.classList.contains("missing")).toBe(true);

    // TODO: make sure it's the info box and not the text "information" at the bottom
    const infoNode = graph.getByText("Info");
    // it should be defined
    expect(infoNode).toBeDefined();
    // Does the infoBox correspond to the correct course?
    expect(infoNode.id).toEqual("csc104-tooltip-text");
    // Unhover over the course
    fireEvent.mouseOut(courseNode);
    // Is the course takeable again? Did the infoBox disappear?
    expect(courseNode.classList.contains("takeable")).toBe(true);
    // TODO: Asynchrony: mock and wait.
    // infoNode = graph.getByText("Info")
    // expect(infoNode).not.toBeDefined()
  });

  it("should sets attributes correctly when clicking on course", () => {
    const courseNode = graph.getByText("CSC104").parentNode;
    // Ensure mouse is not hovering over the course
    fireEvent.mouseOut(courseNode);
    // Is the course takeable?
    expect(courseNode.classList.contains("takeable")).toBe(true);
    // Click on the course.
    // TODO: redundant to check whether it is missing before clicking?
    fireEvent.click(courseNode);
    // Is the course active?
    // TODO: mock callback and check whether it is called with correct arguments
    expect(courseNode.classList.contains("active")).toBe(true);
    // const nodeOnClick - jest.fn()
    // expect(nodeOnClick).toHaveBeenCalledTimes(1)
    // Click the course again
    fireEvent.click(courseNode);
    // Was the course deselected and once again takeable.
    expect(courseNode.classList.contains("takeable")).toBe(true);
  });
});
