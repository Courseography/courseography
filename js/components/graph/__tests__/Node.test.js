import React from "react";
import { shallow } from "enzyme";
import { cleanup, fireEvent } from "react-testing-library";
import Node from "../Node";
import setupGraph from "./setupGraph";

afterEach(cleanup);

/**
 * CSS classes are only initialized after a mouseOut or a click event
 *
 * @param {React.Component.Node} node
 */
function initializeNode(node) {
  fireEvent.mouseOver(node);
  fireEvent.mouseOut(node);
}

describe("Node initialization", () => {
  it("should only be initialized with the 'hybrid' or 'node' CSS class", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;
    expect(hybrid101.classList.contains("hybrid")).toBe(true);
    expect(hybrid101.classList.length).toBe(1);

    const aaa100 = graph.getByText("AAA100").parentNode;
    expect(aaa100.classList.contains("node")).toBe(true);
    expect(aaa100.classList.length).toBe(1);
  });

  it("has a new CSS class when hovered over", async () => {
    const graph = await setupGraph();
    const aaa100 = graph.getByText("AAA100").parentNode;
    expect(aaa100.classList.contains("node")).toBe(true);
    expect(aaa100.classList.length).toBe(1);

    fireEvent.mouseOver(aaa100);
    expect(aaa100.classList.length).toBe(1);

    fireEvent.mouseOut(aaa100);
    expect(aaa100.classList.length).toBe(2);
    expect(aaa100.classList.contains("takeable")).toBe(true);

    fireEvent.mouseOver(aaa100);
    expect(aaa100.classList.contains("missing")).toBe(true);
    expect(aaa100.classList.length).toBe(2);
  });

  it("has a new CSS class when clicked over", async () => {
    const graph = await setupGraph();
    const aaa100 = graph.getByText("AAA100").parentNode;
    expect(aaa100.classList.contains("node")).toBe(true);
    expect(aaa100.classList.length).toBe(1);

    fireEvent.click(aaa100);
    expect(aaa100.classList.length).toBe(2);
    expect(aaa100.classList.contains("active")).toBe(true);

    fireEvent.click(aaa100);
    expect(aaa100.classList.length).toBe(2);
    expect(aaa100.classList.contains("takeable")).toBe(true);
  });
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

  it("should only be initialized with one CSS class: hybrid", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;
    expect(hybrid101.classList.contains("hybrid")).toBe(true);
    expect(hybrid101.classList.length).toBe(1);
  });

  it("shouldn't do anything when you hover or click it", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;

    fireEvent.click(hybrid101);
    expect(document.getElementById("fcecount").textContent).toBe(
      "FCE Count: 0"
    );
    expect(hybrid101.classList.length).toBe(1);
    fireEvent.click(hybrid101);
    expect(document.getElementById("fcecount").textContent).toBe(
      "FCE Count: 0"
    );
    expect(hybrid101.classList.length).toBe(1);
    fireEvent.mouseOver(hybrid101);
    expect(document.getElementById("fcecount").textContent).toBe(
      "FCE Count: 0"
    );
    expect(hybrid101.classList.length).toBe(1);
    fireEvent.mouseOut(hybrid101);
    expect(document.getElementById("fcecount").textContent).toBe(
      "FCE Count: 0"
    );
    expect(hybrid101.classList.length).toBe(1);
  });
  it("should be 'active' when its pre-req parent(s) are met, and 'inactive' otherwise", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;
    const aaa101 = graph.getByText("AAA101").parentNode;

    fireEvent.click(aaa101);

    expect(hybrid101.classList.contains("active")).toBe(true);
    expect(aaa101.classList.contains("active")).toBe(true);
    fireEvent.click(aaa101);
    expect(hybrid101.classList.contains("inactive")).toBe(true);
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

  it("should render Node component properly with proper course code", async () => {
    const graph = await setupGraph();
    const courseTextNode = graph.getByText("AAA100");
    expect(courseTextNode.parentNode.id.toUpperCase()).toBe(
      courseTextNode.innerHTML
    );
  });

  it("should be 'takeable' if unselected and you have the prerequisites", async () => {
    const graph = await setupGraph();
    const aaa101 = graph.getByText("AAA101").parentNode;
    const aaa201 = graph.getByText("AAA201").parentNode;
    initializeNode(aaa201);
    fireEvent.click(aaa101);
    expect(aaa201.classList.contains("takeable")).toBe(true);
  });
  describe("Unselected Course Node", async () => {
    it('should be "missing" when unselected and hovered over', async () => {
      const graph = await setupGraph();
      const aaa100 = graph.getByText("AAA100").parentNode;
      initializeNode(aaa100);
      fireEvent.mouseOver(aaa100);
      expect(aaa100.classList.contains("missing")).toBe(true);
    });

    it("should be 'active' if it's 1) selected and 2) you have the pre-reqs", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      fireEvent.click(aaa101);
      expect(aaa101.classList.contains("active")).toBe(true);
    });
  });

  describe("Selected Course Node", async () => {
    it("should black when pre-reqs are met", async () => {
      const graph = await setupGraph();

      const aaa100 = graph.getByText("AAA100").parentNode;
      fireEvent.click(aaa100);
      expect(aaa100.classList.contains("active")).toBe(true);
    });

    it("should be red with unmet pre-reqs", async () => {
      const graph = await setupGraph();
      const aaa201 = graph.getByText("AAA201").parentNode;
      fireEvent.click(aaa201);
      expect(aaa201.classList.contains("overridden")).toBe(true);
    });

    it("when hovered, should highlight all unmet pre-reqs", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      const aaa102 = graph.getByText("AAA102").parentNode;
      const aaa303 = graph.getByText("AAA303").parentNode;
      initializeNode(aaa101);
      initializeNode(aaa201);
      initializeNode(aaa102);
      initializeNode(aaa303);

      fireEvent.mouseOver(aaa303);
      expect(aaa101.classList.contains("missing")).toBe(true);
      expect(aaa102.classList.contains("missing")).toBe(true);
      expect(aaa201.classList.contains("missing")).toBe(true);
      expect(aaa303.classList.contains("missing")).toBe(true);
    });
    it("when clicking a node with unmet prereqs, it shouldn't affect other nodes", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      const aaa102 = graph.getByText("AAA102").parentNode;
      const aaa303 = graph.getByText("AAA303").parentNode;
      initializeNode(aaa101);
      initializeNode(aaa201);
      initializeNode(aaa102);
      initializeNode(aaa303);
      expect(aaa101.classList.contains("takeable")).toBe(true);
      expect(aaa102.classList.contains("takeable")).toBe(true);
      expect(aaa201.classList.contains("inactive")).toBe(true);
      expect(aaa303.classList.contains("inactive")).toBe(true);
      fireEvent.click(aaa303);
      expect(aaa101.classList.contains("takeable")).toBe(true);
      expect(aaa102.classList.contains("takeable")).toBe(true);
      expect(aaa201.classList.contains("inactive")).toBe(true);
      expect(aaa303.classList.contains("overridden")).toBe(true);
    });

    it("Pressing a course node should increase the FCE count by 0.5 if it's a half-year course", async () => {
      const graph = await setupGraph();
      const aaa100 = graph.getByText("AAA100");
      const aaa201 = graph.getByText("AAA201");

      fireEvent.click(aaa100);
      expect(document.getElementById("fcecount").textContent).toBe(
        "FCE Count: 0.5"
      );
      fireEvent.click(aaa201);
      expect(document.getElementById("fcecount").textContent).toBe(
        "FCE Count: 1"
      );
      fireEvent.click(aaa100);
      expect(document.getElementById("fcecount").textContent).toBe(
        "FCE Count: 0.5"
      );
      fireEvent.click(aaa201);
      expect(document.getElementById("fcecount").textContent).toBe(
        "FCE Count: 0"
      );
    });
  });
});
