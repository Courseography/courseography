import { shallow } from "enzyme"
import { Graph } from "../Graph"

describe("EdgeGroup", () => {
  it("should match shallow snapshot", () => {
    const params = {
      edgesJSON: {},
      edgesStatus: {},
    }

    const graph = new Graph({})
    const component = shallow(
      graph.renderEdgeGroup(params.edgesJSON, params.edgesStatus)
    )

    expect(component).toMatchSnapshot()
  })
})
