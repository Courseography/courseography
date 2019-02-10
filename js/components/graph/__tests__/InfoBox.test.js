import React from "react";
import { shallow } from "enzyme";

import InfoBox from "../InfoBox";

describe("InfoBox", () => {
  it("InfoBox", () => {
    const infoBoxProps = {
      onClick: null,
      onMouseDown: null,
      onMouseLeave: null
    };
    const component = shallow(<InfoBox {...infoBoxProps} />);
    expect(component).toMatchSnapshot();
  });
});
