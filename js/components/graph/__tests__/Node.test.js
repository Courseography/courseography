import React from "react";
import { shallow } from "enzyme";
import { cleanup, fireEvent } from "react-testing-library";
import Node from "../Node";
import setupGraph from "./setupGraph";

afterEach(cleanup);

describe("Hybrid Node", () => {
  it("should match snapshot", () => {
    const hybridNodeProps = {
      JSON: {
        fill: "#888888",
        graph: 1,
        height: 24,
        id_: "h46",
        pos: [18.168848, 471.586698],
        stroke: "",
        type_: "Hybrid",
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
        width: 65.207497,
      },

      "parents": [],
      "state": {
        "labelsJSON": [],
        "regionsJSON": [],
        "nodesJSON": [{
          "graph": 1,
          "height": 30,
          "type_": "Node",
          "text": [{
            "graph": 1,
            "rId": "text102",
            "text": "AAA101",
            "pos": [596.555968, 25.379107999999995],
            "fill": "",
            "align": "begin"
          }, {
            "graph": 1,
            "rId": "text103",
            "text": "(H101)",
            "pos": [596.555968, 34.379107999999995],
            "fill": "",
            "align": "begin"
          }],
          "width": 65,
          "stroke": "",
          "pos": [594.294128, 10.09390799999999],
          "fill": "#5dd5b8",
          "id_": "aaa101"
        },],
        "hybridsJSON": [{
          "graph": 1,
          "height": 24,
          "type_": "Hybrid",
          "text": [{
            "graph": 1,
            "rId": "text103",
            "text": "(H101)",
            "pos": [596.555968, 59.379107999999995],
            "fill": "",
            "align": "begin"
          }],
          "width": 52.768799,
          "stroke": "",
          "pos": [18.168848, 471.586698],
          "fill": "#888888",
          "id_": "h108"
        }],
        "boolsJSON": [],
        "edgesJSON": [],
        "highlightedNodes": [],
        "timeouts": [],
        "fceCount": 0,
        "width": 1217.9941,
        "height": 646.78271,
        "zoomFactor": 1,
        "horizontalPanFactor": 0,
        "verticalPanFactor": 0,
        "mouseDown": false,
        "buttonHover": false,
        "onDraw": false,
      },
      className: "hybrid",
      "svg": {
        "current": {},
        onKeyDown: jest.fn(),
      }
    };
    const wrapper = shallow(<Node {...hybridNodeProps} />);
    expect(wrapper).toMatchSnapshot();
  });

  it("should have the 'hybrid CSS class", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;
    expect(hybrid101.classList.contains("hybrid")).toBe(true);
  });
  it("shouldn't do anything when you hover or click it", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;
    
    // convert DOMTokenList to object so we can deep copy
    const cssClassDeepCopy = {...hybrid101.classList};

    fireEvent.click(hybrid101);
    expect({...hybrid101.classList}).toEqual(cssClassDeepCopy);
    fireEvent.click(hybrid101);
    expect({ ...hybrid101.classList }).toEqual(cssClassDeepCopy);
    fireEvent.mouseOver(hybrid101);
    expect({ ...hybrid101.classList }).toEqual(cssClassDeepCopy);
    fireEvent.mouseOut(hybrid101);
    expect({ ...hybrid101.classList }).toEqual(cssClassDeepCopy);
  });
  it("should be 'active' when its pre-req parent is met, and 'inactive' otherwise", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;
    const aaa101 = graph.getByText("AAA101").parentNode;

    fireEvent.click(aaa101);

    expect(hybrid101.classList.contains("active")).toBe(true);
    fireEvent.click(aaa101);
    expect(hybrid101.classList.contains("inactive")).toBe(true);
  });

  it("should be 'missing' if not 'active' and it's an unmet pre-req of the currently hovered course", async () => {
    const graph = await setupGraph();
    const hybrid101 = graph.getByText("(H101)").parentNode;
    const aaa303 = graph.getByText("AAA303").parentNode

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
  it("should have the CSS class: 'node'", async () => {
    const graph = await setupGraph();
    const aaa100 = graph.getByText("AAA100").parentNode;
    expect(aaa100.classList.contains("node")).toBe(true);
  });

  describe("Unselected Course Node", async () => {
    it("should be 'takeable' if pre-reqs met and 'inactive' otherwise", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      expect(aaa101.classList.contains("takeable")).toBe(true);
      expect(aaa201.classList.contains("inactive")).toBe(true);
    });
    it('should be "missing" when unselected and hovered over', async () => {
      const graph = await setupGraph();
      const aaa100 = graph.getByText("AAA100").parentNode;
      fireEvent.mouseOver(aaa100);
      expect(aaa100.classList.contains("missing")).toBe(true);
    });

    it("when hovered, should set all unmet pre-reqs and itself as 'missing'", async () => {
      const graph = await setupGraph();
      const aaa101 = graph.getByText("AAA101").parentNode;
      const aaa201 = graph.getByText("AAA201").parentNode;
      const aaa102 = graph.getByText("AAA102").parentNode;
      const aaa303 = graph.getByText("AAA303").parentNode;

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
    describe("selected course with un-met pre-reqs", async () => {
      it("should be 'overridden' (red border)", async () => {
        const graph = await setupGraph();
        const aaa201 = graph.getByText("AAA201").parentNode;
        fireEvent.click(aaa201);
        expect(aaa201.classList.contains("overridden")).toBe(true);
      });

      it("that is not hovered over doesn't change the state of the un-met pre-reqs", async () => {
        const graph = await setupGraph();
        const aaa101 = graph.getByText("AAA101").parentNode;
        const aaa201 = graph.getByText("AAA201").parentNode;
        const aaa102 = graph.getByText("AAA102").parentNode;
        const aaa303 = graph.getByText("AAA303").parentNode;
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

      it("is a satisfied dependency/requirement for other courses", async () => {
        // this is especially useful for upper year students who may not wish to click every single course that they've taken
        const graph = await setupGraph();
        const aaa201 = graph.getByText("AAA201").parentNode; // unmet pre-req: AAA101
        const aaa202 = graph.getByText("AAA202").parentNode;

        expect(aaa202.classList.contains("inactive")).toBe(true);
        fireEvent.click(aaa201);
        expect(aaa202.classList.contains("takeable")).toBe(true);
      });

      it("that is hovered over will be 'missing'", async () => {
        const graph = await setupGraph();
        const aaa303 = graph.getByText("AAA303").parentNode;
        fireEvent.click(aaa303);
        expect(aaa303.classList.contains("overridden")).toBe(true);

        fireEvent.mouseOver(aaa303);
        expect(aaa303.classList.contains("missing")).toBe(true);
      });
    });

    it("Selecting a course node should always increase the FCE count by 0.5, (currently no support for full-year courses)", async () => {
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
