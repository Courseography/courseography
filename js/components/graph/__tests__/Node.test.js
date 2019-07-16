import React from "react";
import { shallow } from "enzyme";
import { fireEvent } from "@testing-library/react";
import Node from "../Node";
import TestGraph from "./TestGraph";

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
    const wrapper = shallow(<Node {...hybridNodeProps} />);
    expect(wrapper).toMatchSnapshot();
  });

  it("should have the 'hybrid' CSS class", async () => {
    const graph = await TestGraph.build();
    const hybrid101 = graph.getByTestId("h101");
    expect(hybrid101.classList.contains("hybrid")).toBe(true);
  });
  it("shouldn't do anything when you hover or click it", async () => {
    const graph = await TestGraph.build();
    const hybrid101 = graph.getByTestId("h101");

    // convert DOMTokenList to object so we can deep copy
    const cssClassDeepCopy = JSON.parse(JSON.stringify(hybrid101.classList));

    fireEvent.click(hybrid101);
    // need to convert the classList into an object on order to use Jest's compare
    expect({ ...hybrid101.classList }).toEqual(cssClassDeepCopy);
    fireEvent.click(hybrid101);
    expect({ ...hybrid101.classList }).toEqual(cssClassDeepCopy);
    fireEvent.mouseOver(hybrid101);
    expect({ ...hybrid101.classList }).toEqual(cssClassDeepCopy);
    fireEvent.mouseOut(hybrid101);
    expect({ ...hybrid101.classList }).toEqual(cssClassDeepCopy);
  });
  it("should be 'inactive' when it's prereq parent is NOT met", async () => {
    const graph = await TestGraph.build();
    const hybrid101 = graph.getByTestId("h101");
    expect(hybrid101.classList.contains("inactive")).toBe(true);
  });
  it("should be 'active' when its prereq parent is met", async () => {
    const graph = await TestGraph.build();
    const hybrid101 = graph.getByTestId("h101");
    const aaa101 = graph.getByTestId("aaa101");
    fireEvent.click(aaa101);
    expect(hybrid101.classList.contains("active")).toBe(true);
  });

  it("should be 'missing' if not 'active' and it's an unmet prereq of the currently hovered course", async () => {
    const graph = await TestGraph.build();
    const hybrid101 = graph.getByTestId("h101");
    const aaa303 = graph.getByTestId("aaa303");

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
    const graph = await TestGraph.build();
    const aaa100 = graph.getByTestId("aaa100");
    expect(aaa100.classList.contains("node")).toBe(true);
  });

  describe("Unselected Course Node", () => {
    it("should be 'takeable' if it has no prereqs", async () => {
      const graph = await TestGraph.build();
      const aaa101 = graph.getByTestId("aaa101");
      expect(aaa101.classList.contains("takeable")).toBe(true);
    });
    it("should be 'inactive' if the prereqs are NOT met", async () => {
      const graph = await TestGraph.build();
      const aaa201 = graph.getByTestId("aaa201");
      expect(aaa201.classList.contains("inactive")).toBe(true);
    });
    it("should be 'takeable' if the prereqs are met", async () => {
      const graph = await TestGraph.build();
      const aaa101 = graph.getByTestId("aaa101");
      const aaa201 = graph.getByTestId("aaa201");
      fireEvent.click(aaa101); // the only prereq for AAA201
      expect(aaa201.classList.contains("takeable")).toBe(true);
    });
    it('should be "missing" when unselected and hovered over', async () => {
      const graph = await TestGraph.build();
      const aaa100 = graph.getByTestId("aaa100");
      fireEvent.mouseOver(aaa100);
      expect(aaa100.classList.contains("missing")).toBe(true);
    });

    it("when hovered, should set all unmet prereqs and itself as 'missing'", async () => {
      const graph = await TestGraph.build();
      const aaa101 = graph.getByTestId("aaa101");
      const aaa201 = graph.getByTestId("aaa201");
      const aaa102 = graph.getByTestId("aaa102");
      const aaa303 = graph.getByTestId("aaa303");

      fireEvent.mouseOver(aaa303);
      expect(aaa101.classList.contains("missing")).toBe(true);
      expect(aaa102.classList.contains("missing")).toBe(true);
      expect(aaa201.classList.contains("missing")).toBe(true);
      expect(aaa303.classList.contains("missing")).toBe(true);
    });
  });

  describe("Selected Course Node", () => {
    it("with met prereqs should 'active'", async () => {
      const graph = await TestGraph.build();
      const aaa100 = graph.getByTestId("aaa100");
      fireEvent.click(aaa100);
      expect(aaa100.classList.contains("active")).toBe(true);
    });
    describe("selected course with un-met prereqs", () => {
      it("should be 'overridden' (if you don't hover over it)", async () => {
        const graph = await TestGraph.build();
        const aaa201 = graph.getByTestId("aaa201");
        fireEvent.click(aaa201);
        expect(aaa201.classList.contains("overridden")).toBe(true);
      });

      it("'overridden' course counts as a satisfied prereq (acts like 'active')", async () => {
        const graph = await TestGraph.build();
        const aaa201 = graph.getByTestId("aaa201");
        const aaa202 = graph.getByTestId("aaa202");
        fireEvent.click(aaa201);
        expect(aaa201.classList.contains("overridden")).toBe(true);
        expect(aaa202.classList.contains("takeable")).toBe(true);
      });

      it("hovering over a course with an 'overridden' prereq does not mark the 'overridden' unmet prereqs as 'missing'", async () => {
        const graph = await TestGraph.build();
        const aaa101 = graph.getByTestId("aaa101");
        const aaa201 = graph.getByTestId("aaa201");
        const aaa202 = graph.getByTestId("aaa202");
        fireEvent.click(aaa201);
        fireEvent.mouseOver(aaa202);
        // not missing!
        expect(aaa101.classList.contains("takeable")).toBe(true);
      });

      it("selected and hovered over: sets itself and all unmet prereqs to be 'missing'", async () => {
        const graph = await TestGraph.build();
        const aaa101 = graph.getByTestId("aaa101");
        const aaa201 = graph.getByTestId("aaa201");
        const aaa102 = graph.getByTestId("aaa102");
        const aaa303 = graph.getByTestId("aaa303");
        fireEvent.click(aaa303);

        expect(aaa101.classList.contains("takeable")).toBe(true);
        expect(aaa102.classList.contains("takeable")).toBe(true);
        expect(aaa201.classList.contains("inactive")).toBe(true);
        expect(aaa303.classList.contains("overridden")).toBe(true);

        fireEvent.mouseOver(aaa303);

        expect(aaa101.classList.contains("missing")).toBe(true);
        expect(aaa102.classList.contains("missing")).toBe(true);
        expect(aaa201.classList.contains("missing")).toBe(true);
        expect(aaa303.classList.contains("missing")).toBe(true);
      });

      it("selected + hovered over + unmet prereqs: 'missing'", async () => {
        const graph = await TestGraph.build();
        const aaa303 = graph.getByTestId("aaa303");
        fireEvent.click(aaa303);
        expect(aaa303.classList.contains("overridden")).toBe(true);

        fireEvent.mouseOver(aaa303);
        expect(aaa303.classList.contains("missing")).toBe(true);
      });
    });

    it("Selecting a course node should always increase the FCE count by 0.5, (currently no support for full-year courses)", async () => {
      const graph = await TestGraph.build();
      const aaa100 = graph.getByTestId("aaa100");
      const aaa201 = graph.getByTestId("aaa201");

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
