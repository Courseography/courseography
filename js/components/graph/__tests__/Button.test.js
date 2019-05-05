import React from "react";
import { shallow } from "enzyme";

import Button from "../Button";

describe("Button", () => {
  it("should match shallow snapshot", () => {
    const buttonProps = {
      divId: "reset-button",
      text: "Reset",
      disabled: true
    };
    const component = shallow(<Button {...buttonProps} />);
    expect(component).toMatchSnapshot();
  });
});
