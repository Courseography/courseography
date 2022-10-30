import React from "react"
import { shallow } from "enzyme"

import BoolGroup from "../BoolGroup"

describe("BoolGroup", () => {
  it("BoolGroup should match shallow snapshot", () => {
    const boolGroupProps = {
      boolsJSON: [],
      edgesJSON: [],
      boolsStatus: {'bool1': 'inactive', 'bool2': 'inactive'},
      svg: null,
    }
    const component = shallow(<BoolGroup {...boolGroupProps} />)
    expect(component).toMatchSnapshot()
  })
})
