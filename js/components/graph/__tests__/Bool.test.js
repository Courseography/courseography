import React from "react"
import { shallow } from "enzyme"
import Bool from "../Bool"
import { fireEvent } from "@testing-library/react"
import TestGraph from "./TestGraph"

describe("Bool", () => {
  it("should already have two classes when instantated by Graph", async () => {
    const graph = await TestGraph.build()
    const andBool = graph.getNodeByText("and")
    const orBool = graph.getNodeByText("or")

    expect(andBool.classList[0]).toBe("bool")
    expect(andBool.classList[1]).toBe("inactive")
    expect(orBool.classList[0]).toBe("bool")
    expect(orBool.classList[1]).toBe("inactive")
  })
})

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
            text: "and",
          },
        ],
        type_: "BoolNode",
        width: 19.7600002,
      },
      childs: ["csc369", "csc358", "csc458", "csc385", "csc367"],
      className: "bool",
      inEdges: ["p39", "p40"],
      logicalType: "and",
      outEdges: ["p41", "p42", "p43", "p59", "p82"],
      parents: ["csc209", "csc258"],
      svg: {},
    }
    const wrapper = shallow(<Bool {...boolProps} />)
    expect(wrapper).toMatchSnapshot()
  })

  it("should not do anything when you hover or click on it", async () => {
    const graph = await TestGraph.build()
    const andBool = graph.getNodeByText("and")

    expect(andBool.classList.contains("inactive")).toBe(true)

    fireEvent.mouseOver(andBool)
    expect(andBool.classList.contains("inactive")).toBe(true)

    fireEvent.click(andBool)
    expect(andBool.classList.contains("inactive")).toBe(true)
  })
  it("AND should become selected when its prereq parents are satisfied", async () => {
    const graph = await TestGraph.build()
    const andBool = graph.getNodeByText("and")
    const aaa102 = graph.getByTestId("aaa102")
    const aaa201 = graph.getByTestId("aaa201")
    const aaa303 = graph.getByTestId("aaa303")

    // AAA201 and AAA102 => AAA303
    expect(andBool.classList.contains("inactive")).toBe(true)
    fireEvent.click(aaa102)
    expect(andBool.classList.contains("inactive")).toBe(true)
    fireEvent.click(aaa201)
    expect(andBool.classList.contains("active")).toBe(true)
    expect(aaa303.classList.contains("takeable")).toBe(true)
  })
  it("AND should have the missing class when the mouse is hovering over a child class.", async () => {
    const graph = await TestGraph.build()
    const andBool = graph.getNodeByText("and")
    const aaa303 = graph.getByTestId("aaa303")

    // AAA201 and AAA102 => AAA303
    expect(andBool.classList.contains("inactive")).toBe(true)

    // mouseout or clicking triggers code to set the CSS class
    fireEvent.mouseOver(aaa303)
    fireEvent.mouseOut(aaa303)
    fireEvent.mouseOver(aaa303)
    expect(andBool.classList.contains("missing")).toBe(true)
  })
})

describe("OR Bool", () => {
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
            text: "or",
          },
        ],
        type_: "BoolNode",
        width: 19.7600002,
      },
      childs: ["csc369", "csc358", "csc458", "csc385", "csc367"],
      className: "bool",
      inEdges: ["p39", "p40"],
      logicalType: "or",
      outEdges: ["p41", "p42", "p43", "p59", "p82"],
      parents: ["csc209", "csc258"],
      svg: {},
    }
    const wrapper = shallow(<Bool {...boolProps} />)
    expect(wrapper).toMatchSnapshot()
  })

  it("should not do anything when you hover or click on it", async () => {
    const graph = await TestGraph.build()
    const orBool = graph.getNodeByText("or")

    expect(orBool.classList.contains("inactive")).toBe(true)

    fireEvent.mouseOver(orBool)
    expect(orBool.classList.contains("inactive")).toBe(true)

    fireEvent.click(orBool)
    expect(orBool.classList.contains("inactive")).toBe(true)
  })
  it("or should become selected when its prereq parents are satisfied", async () => {
    const graph = await TestGraph.build()
    const orBool = graph.getNodeByText("or")
    const aaa102 = graph.getByTestId("aaa102")
    const aaa201 = graph.getByTestId("aaa201")
    const aaa202 = graph.getByTestId("aaa202")

    // AAA201 or AAA102 => AAA202
    expect(orBool.classList.contains("inactive")).toBe(true)

    fireEvent.click(aaa102)
    expect(orBool.classList.contains("active")).toBe(true)
    expect(aaa202.classList.contains("takeable")).toBe(true)

    fireEvent.click(aaa201)
    expect(orBool.classList.contains("active")).toBe(true)
    expect(aaa202.classList.contains("takeable")).toBe(true)
  })
  it("or should have the missing class when the mouse is hovering over a child class.", async () => {
    const graph = await TestGraph.build()
    const orBool = graph.getNodeByText("or")
    const aaa202 = graph.getByTestId("aaa202")

    // AAA201 or AAA102 => AAA202
    expect(orBool.classList.contains("inactive")).toBe(true)
    fireEvent.mouseOver(aaa202)
    fireEvent.mouseOut(aaa202)
    fireEvent.mouseOver(aaa202)
    expect(orBool.classList.contains("missing")).toBe(true)
  })
})
