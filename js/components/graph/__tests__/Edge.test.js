import React from "react";
import { shallow } from "enzyme";
import Edge from "../Edge";
import setupGraph from "./setupGraph";
import { cleanup, fireEvent } from "react-testing-library";

afterEach(cleanup);

describe("Edge", () => {

  function getPath(graph, name) {
    const mapping = {
      '102-201': 0
      // TODO: get all of the paths here
    };
    if (mapping[name] === undefined) {
      throw new Error(`Path "${name}" not found!`);
    }
    return graph.container.getElementsByTagName("path")[mapping[name]];
  }
  it("should match shallow snapshot", () => {
    const edgeProps = {
      className: "path",
      edgeID: "p1",
      points: [[497.651848, 69.09890799999998], [497.651848, 130.885308]],
      source: "csc165240",
      target: "csc236240",
      updateEdgeStatus: null,
      svg: {}
    };
    const component = shallow(<Edge {...edgeProps} />);
    expect(component).toMatchSnapshot();
  });

  it("should be initialized with the class 'inactive'", async () => {
    await setupGraph();
    const paths = document.getElementsByTagName("path");
    for (let i = 0; i < paths.length; i += 1) {
      expect(paths[i].classList.contains("inactive")).toBe(true);
      expect(paths[i].classList.contains("path")).toBe(true);
    }
  });

  it("with met pre-req should be active", async () => {
    const graph = await setupGraph();
    const aaa102 = graph.getByText("AAA102");
    expect(getPath(graph, '102-201').classList.contains("inactive")).toBe(true);

    fireEvent.click(aaa102);
    jest.runOnlyPendingTimers();
    console.log(getPath(graph, '102-201'))
    // expect(getPath(graph, '102-201').classList.contains("takeable")).toBe(true);
    // console.log(getPath(graph, '102-201').classList)
    // expect(getPath(graph, '102-201').classList.contains("active")).toBe(true);
  });

  it("with unmet re-req with selected child class should still be inactive", () => {});

  it("with met pre-reqs should be takeable", async () => {
    const graph = await setupGraph();
    const aaa201 = graph.getByText("AAA201").parentNode;
    // TODO: how to select an edge with RTL?
    // const edge102 = graph.getBy???

    fireEvent.click(aaa201);
    // TODO: expect(edge201.classList.contains('takeable')).toBeTruthy();
    console.log(aaa201.classList)
  });

  it("with met pre-req and a selected child class should be active", () => {

  });
});
