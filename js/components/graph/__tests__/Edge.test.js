import React from "react"
import { shallow } from "enzyme"
import TestGraph from "./TestGraph"
import { fireEvent } from "@testing-library/react"

import Edge from "../Edge"

describe("Edge", () => {
  it("should match shallow snapshot", () => {
    const edgeProps = {
      className: "path",
      edgeID: "p1",
      points: [
        [497.651848, 69.09890799999998],
        [497.651848, 130.885308],
      ],
      source: "csc165240",
      target: "csc236240",
      updateEdgeStatus: null,
      edgeStatus: "inactive",
      svg: {},
    }
    const component = shallow(<Edge {...edgeProps} />)
    expect(component).toMatchSnapshot()
  })

  describe("Clicking course nodes", () => {
    it("unselected source and destination => 'inactive' Edge", async () => {
      const graph = await TestGraph.build()
      const path_h101_201 = graph.getByTestId("h101->aaa201")
      expect(path_h101_201.classList.contains("inactive")).toBe(true)
    })
    it("with selected source and unselected destination => 'takeable' Edge", async () => {
      const graph = await TestGraph.build()
      const aaa101 = graph.getByTestId("aaa101")
      const path_101_201 = graph.getByTestId("h101->aaa201")

      fireEvent.click(aaa101)
      expect(path_101_201.classList.contains("takeable")).toBe(true)
    })

    it("with unselected source with selected destination => 'inactive' Edge", async () => {
      const graph = await TestGraph.build()
      const aaa201 = graph.getByTestId("aaa201")
      const path_101_201 = graph.getByTestId("h101->aaa201")
      expect(path_101_201.classList.contains("inactive")).toBe(true)

      fireEvent.click(aaa201)
      expect(path_101_201.classList.contains("inactive")).toBe(true)
    })

    it("with selected source and destination => 'active' Edge", async () => {
      const graph = await TestGraph.build()
      const aaa101 = graph.getByTestId("aaa101")
      const aaa201 = graph.getByTestId("aaa201")
      const path_101_201 = graph.getByTestId("h101->aaa201")
      fireEvent.click(aaa101)
      fireEvent.click(aaa201)

      expect(path_101_201.classList.contains("active")).toBe(true)
    })
  })

  describe("hovering behaviour", () => {
    describe("hovering over source does nothing", () => {
      it("hovered and unselected source and unselected destination => stays 'inactive'", async () => {
        const graph = await TestGraph.build()
        const aaa101 = graph.getByTestId("aaa101")
        const path_101_201 = graph.getByTestId("h101->aaa201")

        expect(path_101_201.classList.contains("inactive")).toBe(true)
        fireEvent.mouseOver(aaa101)
        expect(path_101_201.classList.contains("inactive")).toBe(true)
      })

      it("hovered and selected source and unselected destination => stays 'takeable'", async () => {
        const graph = await TestGraph.build()
        const aaa101 = graph.getByTestId("aaa101")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        fireEvent.click(aaa101)
        expect(path_101_201.classList.contains("takeable")).toBe(true)
        fireEvent.mouseOver(aaa101)
        expect(path_101_201.classList.contains("takeable")).toBe(true)
      })

      it("hovered and unselected source and selected destination => stays 'active'", async () => {
        const graph = await TestGraph.build()
        const aaa101 = graph.getByTestId("aaa101")
        const aaa201 = graph.getByTestId("aaa201")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        fireEvent.click(aaa201)
        expect(path_101_201.classList.contains("inactive")).toBe(true)
        fireEvent.mouseOver(aaa101)
        expect(path_101_201.classList.contains("inactive")).toBe(true)
      })

      it("(hovered and selected source) and selected destination => stays 'active'", async () => {
        const graph = await TestGraph.build()
        const aaa101 = graph.getByTestId("aaa101")
        const aaa201 = graph.getByTestId("aaa201")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        fireEvent.click(aaa101)
        fireEvent.click(aaa201)
        expect(path_101_201.classList.contains("active")).toBe(true)
        fireEvent.mouseOver(aaa101)
        expect(path_101_201.classList.contains("active")).toBe(true)
      })
    })

    describe("hovering over destination", () => {
      it("unselected source and (unselected and hovered destination) => edge should transition from 'inactive' to 'missing'", async () => {
        const graph = await TestGraph.build()
        const aaa201 = graph.getByTestId("aaa201")
        const path_101_201 = graph.getByTestId("h101->aaa201")

        expect(path_101_201.classList.contains("inactive")).toBe(true)
        fireEvent.mouseOver(aaa201)
        expect(path_101_201.classList.contains("missing")).toBe(true)
      })
      it("selected source and (unselected and hovered destination) => edge remains 'takeable'", async () => {
        const graph = await TestGraph.build()
        const aaa101 = graph.getByTestId("aaa101")
        const aaa201 = graph.getByTestId("aaa201")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        fireEvent.click(aaa101)
        expect(path_101_201.classList.contains("takeable")).toBe(true)
        fireEvent.mouseOver(aaa201)
        expect(path_101_201.classList.contains("takeable")).toBe(true)
      })
      it("unselected source and (selected and hovered destination) => edge should transition to inactive 'missing'", async () => {
        const graph = await TestGraph.build()
        const aaa201 = graph.getByTestId("aaa201")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        fireEvent.click(aaa201)
        expect(path_101_201.classList.contains("inactive")).toBe(true)
        fireEvent.mouseOver(aaa201)
        expect(path_101_201.classList.contains("missing")).toBe(true)
      })
      it("unselected source and (selected and hovered destination) => edge and all unmet and inactive prereqs should be 'missing'", async () => {
        const graph = await TestGraph.build()
        const aaa303 = graph.getByTestId("aaa303")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        const path_201_and = graph.getByTestId("aaa201->bool1")
        const path_102_and = graph.getByTestId("aaa102->bool1")
        const path_and_303 = graph.getByTestId("bool1->aaa303")

        fireEvent.click(aaa303) // selected node with missing prereqs

        expect(path_101_201.classList.contains("inactive")).toBe(true)
        expect(path_201_and.classList.contains("inactive")).toBe(true)
        expect(path_102_and.classList.contains("inactive")).toBe(true)
        expect(path_and_303.classList.contains("inactive")).toBe(true)

        fireEvent.mouseOver(aaa303)
        expect(path_101_201.classList.contains("missing")).toBe(true)
        expect(path_201_and.classList.contains("missing")).toBe(true)
        expect(path_102_and.classList.contains("missing")).toBe(true)
        expect(path_and_303.classList.contains("missing")).toBe(true)
      })

      it("selected source and selected destination, hovering over the destination will have the Edge remain 'active'", async () => {
        const graph = await TestGraph.build()
        const aaa101 = graph.getByTestId("aaa101")
        const aaa201 = graph.getByTestId("aaa201")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        fireEvent.click(aaa101)
        fireEvent.click(aaa201)
        expect(path_101_201.classList.contains("active")).toBe(true)
        fireEvent.mouseOver(aaa201)
        expect(path_101_201.classList.contains("active")).toBe(true)
      })
    })
  })
})
