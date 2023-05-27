import { shallow } from "enzyme"
import { Graph } from "../Graph"

describe("NodeGroup", () => {
  it("should match shallow snapshot", () => {
    const params = {
      nodeClick: jest.fn(),
      nodeMouseEnter: jest.fn(),
      nodeMouseLeave: jest.fn(),
      nodeMouseDown: jest.fn(),
      onKeyDown: jest.fn(),
      onWheel: jest.fn(),
      nodesStatus: {},
      nodesJSON: {},
      hybridsJSON: {},
      highlightedNodes: [],
      edgesJSON: [],
      connections: {},
      nodeDropshadowFilter: "",
    }

    const graph = new Graph({})
    const component = shallow(
      graph.renderNodeGroup(
        params.nodeClick,
        params.nodeMouseEnter,
        params.nodeMouseLeave,
        params.nodeMouseDown,
        params.onKeyDown,
        params.onWheel,
        params.nodesStatus,
        params.nodesJSON,
        params.hybridsJSON,
        params.highlightedNodes,
        params.edgesJSON,
        params.connections,
        params.nodeDropshadowFilter
      )
    )

    expect(component).toMatchSnapshot()
  })
})
