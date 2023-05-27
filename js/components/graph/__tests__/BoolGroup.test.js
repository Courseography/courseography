import { shallow } from "enzyme"
import { Graph } from "../Graph"

describe("BoolGroup", () => {
  it("BoolGroup should match shallow snapshot", () => {
    const params = {
      boolsJSON: {},
      boolsStatus: { bool1: "inactive", bool2: "inactive" },
      connections: {},
    }

    const graph = new Graph({})
    const component = shallow(
      graph.renderBoolGroup(params.boolsJSON, params.boolsStatus, params.connections)
    )
    // const component = shallow(<BoolGroup {...boolGroupProps} />)
    expect(component).toMatchSnapshot()
  })
})
