import React from "react";
import { shallow } from "enzyme";
import Edge from "../Edge";
import setupGraph from "./setupGraph";
import { cleanup, fireEvent } from "react-testing-library";

afterEach(cleanup);

describe("Edge", () => {
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

  it("with unmet pre-req should be inactive", () => {});

  it("with unmet re-req with selected child class should still be inactive", () => {});

  it("with met pre-reqs should be takeable", async () => {
    const graph = await setupGraph();
    const aaa101 = graph.getByText("AAA101").parentNode;
    // TODO: how to select an edge with RTL?
    // const edge101 = graph.getBy???

    fireEvent.click(aaa101);
    // TODO: expect(edge101.classList.contains('takeable')).toBeTruthy();
  });

  it("with met pre-req and a selected child class should be active", () => {});
});
