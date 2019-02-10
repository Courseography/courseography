import React from "react";
import { shallow } from "enzyme";

import RegionGroup from "../RegionGroup";

describe("RegionGroup", () => {
  it("should match shallow snapshot", () => {
    const props = {
      regionsJSON: [
        {
          graph: 1,
          points: [
            [17.386348, 281.07376883],
            [17.386348, 14.41568908],
            [316.385978, 14.41576883],
            [316.386348, 281.07376883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#6276b9",
          id_: "p83",
          source: "",
          target: ""
        }
      ],
      labelsJSON: [
        {
          graph: 1,
          rId: "tspan4346-9",
          text: "Systems",
          pos: [1088.1677413999998, 201.36453113000002],
          fill: "#000000",
          align: "begin"
        }
      ]
    };

    const wrapper = shallow(<RegionGroup {...props} />);
    expect(wrapper).toMatchSnapshot();
  });
});
