import React from "react";
import { shallow } from "enzyme";
import Bool from "../Bool";
import {
  render,
  cleanup,
  prettyDOM,
  wait,
  fireEvent
} from "react-testing-library";
import Graph from "../Graph";

/**
 * @param {function} done - provided by Jest to indicate the completion of an async operation
 * @returns {Graph}
 */
async function setupGraph() {
  const graphProps = {
    edit: false,
    initialDrawMode: "draw-node",
    initialOnDraw: false,
    start_blank: false
  };

  const graph = render(<Graph {...graphProps} />);
  await wait(() => graph.queryByText("AAA100") !== null);
  expect(graph.queryByText("AAA100")).toBeTruthy();
  return graph;
}

afterEach(cleanup);

describe("AND Bool", () => {
  it("should match shallow snapshot", () => {
    const boolProps = {
      JSON: {
        fill: "",
        graph: 1,
        height: 14.7368002,
        id: "bool1",
        pos: [1020.015148, 243.51310800000002],
        stroke: "",
        text: [
          {
            align: "begin",
            fill: "",
            graph: 1,
            pos: [1012.903868, 246.655709],
            rID: "text512",
            text: "and"
          }
        ],
        type_: "BoolNode",
        width: 19.7600002
      },
      childs: ["csc369", "csc358", "csc458", "csc385", "csc367"],
      className: "bool",
      inEdges: ["p39", "p40"],
      logicalType: "and",
      outEdges: ["p41", "p42", "p43", "p59", "p82"],
      parents: ["csc209", "csc258"],
      svg: {}
    };
    const wrapper = shallow(<Bool {...boolProps} />);
    expect(wrapper).toMatchSnapshot();
  });

  it("should not do anything when you hover or click on it", async () => {
    const graph = await setupGraph();
    const andBool = graph.getByText("and").parentNode;

    expect(andBool.classList.contains("inactive")).toBeTruthy();

    fireEvent.mouseOver(andBool);
    expect(andBool.classList.contains("inactive")).toBeTruthy();

    fireEvent.click(andBool);
    expect(andBool.classList.contains("inactive")).toBeTruthy();
  });
  it("AND should become selected when its pre-req parents are satisfied", async () => {
    const graph = await setupGraph();
    const andBool = graph.getByText("and").parentNode;
    const aaa102 = graph.getByText("AAA102").parentNode;
    const aaa201 = graph.getByText("AAA201").parentNode;
    const aaa303 = graph.getByText("AAA303").parentNode;

    // AAA201 and AAA102 => AAA303
    expect(andBool.classList.contains("inactive")).toBeTruthy();
    fireEvent.click(aaa102);
    expect(andBool.classList.contains("inactive")).toBeTruthy();
    fireEvent.click(aaa201);
    expect(andBool.classList.contains("active")).toBeTruthy();
    expect(aaa303.classList.contains("takeable")).toBeTruthy();
  });
  it.skip("AND should have the missing class when the mouse is hovering over a child class.", async () => {
    const graph = await setupGraph();
    const andBool = graph.getByText("and").parentNode;
    const aaa303 = graph.getByText("AAA303").parentNode;

    // AAA201 and AAA102 => AAA303
    expect(andBool.classList.contains("inactive")).toBeTruthy();
    fireEvent.mouseOver(aaa303);
    console.log(prettyDOM(andBool));
    console.log(prettyDOM(aaa303));
    expect(andBool.classList.contains("missing")).toBeTruthy();
  });
});

describe("OR Bool", () => {
  it("should not do anything when you hover or click on it", async () => {
    const graph = await setupGraph();
    const orBool = graph.getByText("or").parentNode;

    expect(orBool.classList.contains("inactive")).toBeTruthy();

    fireEvent.mouseOver(orBool);
    expect(orBool.classList.contains("inactive")).toBeTruthy();

    fireEvent.click(orBool);
    expect(orBool.classList.contains("inactive")).toBeTruthy();
  });
  it("or should become selected when its pre-req parents are satisfied", async () => {
    const graph = await setupGraph();
    const orBool = graph.getByText("or").parentNode;
    const aaa102 = graph.getByText("AAA102").parentNode;
    const aaa201 = graph.getByText("AAA201").parentNode;
    const aaa202 = graph.getByText("AAA202").parentNode;

    // AAA201 or AAA102 => AAA202
    expect(orBool.classList.contains("inactive")).toBeTruthy();
    
    fireEvent.click(aaa102);
    expect(orBool.classList.contains("active")).toBeTruthy();
    expect(aaa202.classList.contains("takeable")).toBeTruthy();

    fireEvent.click(aaa201);
    expect(orBool.classList.contains("active")).toBeTruthy();
    expect(aaa202.classList.contains("takeable")).toBeTruthy();
  });
  it.skip("or should have the missing class when the mouse is hovering over a child class.", async () => {
    const graph = await setupGraph();
    const orBool = graph.getByText("or").parentNode;
    const aaa202 = graph.getByText("AAA202").parentNode;

    // AAA201 or AAA102 => AAA202
    expect(orBool.classList.contains("inactive")).toBeTruthy();
    fireEvent.mouseOver(aaa202);
    console.log(prettyDOM(orBool));
    console.log(prettyDOM(aaa202));
    expect(orBool.classList.contains("missing")).toBeTruthy();
  });
});
