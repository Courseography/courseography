import React from "react";
import { shallow } from "enzyme";
import InfoBox from "../InfoBox";
import setupGraph from "./setupGraph";
import { cleanup, fireEvent } from "react-testing-library";

afterEach(cleanup);

describe("InfoBox", () => {
  it("should match shallow snapshot", () => {
    const infoBoxProps = {
      onClick: jest.fn(),
      onMouseDown: jest.fn(),
      onMouseLeave: jest.fn()
    };
    const component = shallow(<InfoBox {...infoBoxProps} />);
    expect(component).toMatchSnapshot();
  });

  it("should appear when hovering over a course", async () => {
    const graph = await setupGraph();
    const aaa100 = graph.getByText("AAA100");
    let infoBox = graph.queryByText("Info");

    expect(infoBox).toBe(null);
    fireEvent.mouseOver(aaa100);

    infoBox = graph.getByText("Info").parentNode;
    expect(infoBox.classList.contains("tooltip-group")).toBe(true);
  });
  it("should disappear a second after the the cursor isn't hovered over the course", async done => {
    const graph = await setupGraph();
    const aaa100 = graph.getByText("AAA100");

    fireEvent.mouseOver(aaa100);
    const infoBox = graph.getByText("Info").parentNode;
    expect(infoBox.classList.contains("tooltip-group")).toBe(true);

    fireEvent.mouseOut(aaa100);

    setTimeout(() => {
      expect(graph.queryByText("Info")).toBe(null);
      done();
    }, 1000);
  });
  it("Pressing on the info box should create a new pop up", async done => {
    // TODO: add to the HTTP mock
    const graph = await setupGraph();
    const aaa100 = graph.getByText("AAA100");
    fireEvent.mouseOver(aaa100);
    const infoBox = graph.getByText("Info").parentNode;
    fireEvent.click(infoBox);

    // wait for fake fetch to finish
    setTimeout(() => {
      // expect description in the modal box to appear
      expect(graph.queryByText("AAA100 Computational Thinking")).not.toBe(null);
      done();
    });
  });
});
