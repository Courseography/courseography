import React from "react"
import TestGraph from "./TestGraph"
import { fireEvent, render, act, screen } from "@testing-library/react"
import Edge from "../Edge"

describe("Edge", () => {
  it("should match snapshot", async () => {
    const edgeProps = {
      className: "path",
      edgeID: "p1",
      points: [
        [497.651848, 69.09890799999998],
        [497.651848, 130.885308],
      ],
      source: "csc165240",
      target: "csc236240",
      status: "inactive",
      updateEdgeStatus: null,
      svg: {},
    }
    await act(async () => render(<Edge {...edgeProps} />))
    const edgeTestId = `csc165240->csc236240`
    const edgeElement = screen.getByTestId(edgeTestId)
    expect(edgeElement).toMatchSnapshot()
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

    it("clicking a node changes the status of all out edges", async () => {
      const graph = await TestGraph.build()
      const aaa102 = graph.getByTestId("aaa102")
      const path_102_bool1 = graph.getByTestId("aaa102->bool1")
      const path_102_bool2 = graph.getByTestId("aaa102->bool2")
      const path_bool2_aaa202 = graph.getByTestId("bool2->aaa202")
      fireEvent.click(aaa102)
      expect(path_102_bool1.classList.contains("takeable")).toBe(true)
      expect(path_102_bool2.classList.contains("active")).toBe(true)
      expect(path_bool2_aaa202.classList.contains("takeable")).toBe(true)
    })

    it("clicking a node and clicking it again resets the status of all edges", async () => {
      const graph = await TestGraph.build()
      const aaa102 = graph.getByTestId("aaa102")
      const path_102_bool1 = graph.getByTestId("aaa102->bool1")
      const path_102_bool2 = graph.getByTestId("aaa102->bool2")
      const path_bool2_aaa202 = graph.getByTestId("bool2->aaa202")
      fireEvent.click(aaa102)
      expect(path_102_bool1.classList.contains("takeable")).toBe(true)
      expect(path_102_bool2.classList.contains("active")).toBe(true)
      expect(path_bool2_aaa202.classList.contains("takeable")).toBe(true)
      fireEvent.click(aaa102)
      expect(path_102_bool1.classList.contains("inactive")).toBe(true)
      expect(path_102_bool2.classList.contains("inactive")).toBe(true)
      expect(path_bool2_aaa202.classList.contains("inactive")).toBe(true)
    })

    it("clicking a node with a takeable edge makes that edge active", async () => {
      const graph = await TestGraph.build()
      const aaa102 = graph.getByTestId("aaa102")
      const aaa202 = graph.getByTestId("aaa202")
      const path_bool2_aaa202 = graph.getByTestId("bool2->aaa202")
      fireEvent.click(aaa102)
      expect(path_bool2_aaa202.classList.contains("takeable")).toBe(true)
      fireEvent.mouseEnter(aaa202)
      expect(path_bool2_aaa202.classList.contains("takeable")).toBe(true)
      fireEvent.click(aaa202)
      expect(path_bool2_aaa202.classList.contains("active")).toBe(true)
    })

    it("clicking reset, sets the selected edge with status takeable, inactive", async () => {
      const graph = await TestGraph.build()
      const aaa101 = graph.getByTestId("aaa101")
      const path_101_201 = graph.getByTestId("h101->aaa201")
      fireEvent.click(aaa101)
      expect(path_101_201.classList.contains("takeable")).toBe(true)

      fireEvent.click(graph.getByTestId("test-reset"))
      expect(path_101_201.classList.contains("inactive")).toBe(true)
    })

    it("clicking reset, sets the selected edge with status active, inactive", async () => {
      const graph = await TestGraph.build()
      const aaa101 = graph.getByTestId("aaa101")
      const aaa201 = graph.getByTestId("aaa201")
      const path_101_201 = graph.getByTestId("h101->aaa201")
      fireEvent.click(aaa101)
      fireEvent.click(aaa201)
      expect(path_101_201.classList.contains("active")).toBe(true)

      fireEvent.click(graph.getByTestId("test-reset"))
      expect(path_101_201.classList.contains("inactive")).toBe(true)
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

      it("hover on and hover off should change edge state to missing", async () => {
        const graph = await TestGraph.build()
        const aaa201 = graph.getByTestId("aaa201")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        fireEvent.mouseEnter(aaa201)
        expect(path_101_201.classList.contains("missing")).toBe(true)
        fireEvent.mouseLeave(aaa201)
        expect(path_101_201.classList.contains("inactive")).toBe(true)
      })

      it("hovering over a node should highlight all the missing nodes and not change the active nodes", async () => {
        const graph = await TestGraph.build()
        const aaa202 = graph.getByTestId("aaa202")
        const aaa101 = graph.getByTestId("aaa101")
        const path_bool2_aaa202 = graph.getByTestId("bool2->aaa202")
        const path_aaa102_bool2 = graph.getByTestId("aaa102->bool2")
        const path_aaa201_bool2 = graph.getByTestId("aaa201->bool2")
        const path_101_201 = graph.getByTestId("h101->aaa201")
        fireEvent.click(aaa101)
        expect(path_101_201.classList.contains("takeable")).toBe(true)
        fireEvent.mouseEnter(aaa202)
        expect(path_101_201.classList.contains("takeable")).toBe(true)
        expect(path_bool2_aaa202.classList.contains("missing")).toBe(true)
        expect(path_aaa102_bool2.classList.contains("missing")).toBe(true)
        expect(path_aaa201_bool2.classList.contains("missing")).toBe(true)
      })

      it("hovering over a node with missing edges then clicking it change the edge status to inactive", async () => {
        const graph = await TestGraph.build()
        const aaa202 = graph.getByTestId("aaa202")
        const path_bool2_aaa202 = graph.getByTestId("bool2->aaa202")
        fireEvent.mouseEnter(aaa202)
        expect(path_bool2_aaa202.classList.contains("missing")).toBe(true)
        fireEvent.click(aaa202)
        expect(path_bool2_aaa202.classList.contains("inactive")).toBe(true)
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
  describe("Edge status is restored on reload", () => {
    beforeAll(() => {
      Object.defineProperty(window, "location", {
        writable: true,
        value: { reload: jest.fn() },
      })
    })
    it("should restore edge status", async () => {
      const graph = await TestGraph.build()
      const aaa102 = graph.getByTestId("aaa102")
      const aaa201 = graph.getByTestId("aaa201")
      const aaa102_bool2 = graph.getByTestId("aaa102->bool2")
      const aaa102_bool1 = graph.getByTestId("aaa102->bool1")
      const bool2_aaa202 = graph.getByTestId("bool2->aaa202")
      const aaa201_bool2 = graph.getByTestId("aaa201->bool2")
      const aaa201_bool1 = graph.getByTestId("aaa201->bool1")
      const bool1_aaa303 = graph.getByTestId("bool1->aaa303")
      fireEvent.click(aaa102)
      fireEvent.click(aaa201)
      expect(aaa102_bool2.classList.contains("active")).toBe(true)
      expect(aaa102_bool1.classList.contains("active")).toBe(true)
      expect(bool2_aaa202.classList.contains("takeable")).toBe(true)
      expect(aaa201_bool2.classList.contains("active")).toBe(true)
      expect(aaa201_bool1.classList.contains("active")).toBe(true)
      expect(bool1_aaa303.classList.contains("takeable")).toBe(true)
      window.location.reload()
      expect(window.location.reload).toHaveBeenCalledTimes(1)
      expect(aaa102_bool2.classList.contains("active")).toBe(true)
      expect(aaa102_bool1.classList.contains("active")).toBe(true)
      expect(bool2_aaa202.classList.contains("takeable")).toBe(true)
      expect(aaa201_bool2.classList.contains("active")).toBe(true)
      expect(aaa201_bool1.classList.contains("active")).toBe(true)
      expect(bool1_aaa303.classList.contains("takeable")).toBe(true)
    })
  })
})
