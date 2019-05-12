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
  it("should match snapshot", () => {
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

  it("should be 'missing' if not 'active' and it's an unmet pre-req of the currently hovered course", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;
    const aaa303 = graph.getByText("AAA303").parentNode;
    initializeNode(aaa303);

    fireEvent.mouseOver(aaa303);
    expect(hybrid101.classList.contains("missing")).toBe(true);
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
  it("should only be initialized with one CSS class: node", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("AAA100").parentNode;
    expect(hybrid101.classList.contains("node")).toBe(true);
    expect(hybrid101.classList.length).toBe(1);
  });

  describe("Unselected Course Node", async () => {
    it("should be 'takeable' if pre-reqs met and 'inactive' otherwise", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      initializeNode(aaa101);
      initializeNode(aaa201);
      expect(aaa101.classList.contains("takeable")).toBe(true);
      expect(aaa201.classList.contains("inactive")).toBe(true);
    });
    it('should be "missing" when unselected and hovered over', async () => {
      const graph = await setupGraph();
      const aaa100 = graph.getByText("AAA100").parentNode;
      initializeNode(aaa100);
      fireEvent.mouseOver(aaa100);
      expect(aaa100.classList.contains("missing")).toBe(true);
    });

    it("when hovered, should set all unmet pre-reqs and itself as 'missing'", async () => {
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

      fireEvent.mouseOut(aaa303);
    });
  });

  describe("Selected Course Node", async () => {
    it("with met pre-reqs should 'active' (black border)", async () => {
      const graph = await setupGraph();
      const aaa100 = graph.getByText("AAA100").parentNode;
      fireEvent.click(aaa100);
      expect(aaa100.classList.contains("active")).toBe(true);
    });

    it("with unmet pre-reqs should be 'overridden' (red border)", async () => {
      const graph = await setupGraph();
      const aaa201 = graph.getByText("AAA201").parentNode;
      fireEvent.click(aaa201);
      expect(aaa201.classList.contains("overridden")).toBe(true);
    });

    it("with unmet un-met pre-reqs ('overridden') doesn't affect parent course nodes (the unmet pre-reqs)", async () => {
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

    it("with unmet pre-reqs ('overridden') behaves the same as an 'active' class to a child class", async () => {
      // this is especially useful for upper year students who may not wish to click every single course that they've taken
      const graph = await setupGraph();
      const aaa201 = graph.getByText("AAA201").parentNode; // unmet pre-req: AAA101
      const aaa202 = graph.getByText("AAA202").parentNode;
      initializeNode(aaa202);

      fireEvent.click(aaa201);
      expect(aaa202.classList.contains("takeable")).toBe(true);
    });

    it("hovered with unmet pre-reqs will be 'missing' even if it's selected", async () => {
      const graph = await setupGraph();
      const aaa303 = graph.getByText("AAA303").parentNode;
      fireEvent.click(aaa303);
      expect(aaa303.classList.contains("overridden")).toBe(true);

      fireEvent.mouseOver(aaa303);
      expect(aaa303.classList.contains("missing")).toBe(true);
    });

    it("Selecting a course node should always increase the FCE count by 0.5, (the current behaviour isn't correct)", async () => {
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
