import React from "react";
import { shallow } from "enzyme";

import RegionGroup from "../RegionGroup";

describe("RegionGroup", () => {
  it("RegionGroup", () => {
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
        },
        {
          graph: 1,
          points: [
            [293.386348, 661.0737688300001],
            [293.386348, 428.07354883],
            [315.906348, 428.07354883],
            [315.906348, 366.07376883],
            [127.060348, 366.07376883],
            [127.060348, 661.0737688300001]
          ],
          isRegion: true,
          stroke: "",
          fill: "#708a23",
          id_: "p84",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [315.906348, 14.41576883],
            [592.386348, 14.41576883],
            [592.386558, 256.57454882999997],
            [646.982558, 256.57454882999997],
            [646.982558, 428.07354883],
            [536.571828, 429.08590883],
            [426.320848, 428.07354883],
            [315.906348, 428.07354883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#5ac0ca",
          id_: "p85",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [17.3863510970132, 661.0737688300001],
            [17.3863510970132, 281.07376883],
            [315.247348, 281.07376883],
            [315.247348, 366.07376883],
            [127.060348, 366.07376883],
            [127.060348, 661.0737688300001]
          ],
          isRegion: true,
          stroke: "",
          fill: "#00ff00",
          id_: "p86",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [293.386348, 661.0737688300001],
            [530.558348, 661.0737688300001],
            [530.558348, 428.07354883],
            [293.386348, 428.07354883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#c2f013",
          id_: "p87",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [907.982558, 465.16554883000003],
            [907.982558, 661.0737688300001],
            [529.957608, 661.0737688300001],
            [530.150958, 428.07354883],
            [646.982558, 428.07354883],
            [646.982558, 465.16554883000003]
          ],
          isRegion: true,
          stroke: "",
          fill: "#3520ca",
          id_: "p88",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [592.386558, 142.29076883],
            [592.386348, 256.57454882999997],
            [646.982558, 256.57454882999997],
            [646.982558, 465.16554883000003],
            [907.982558, 465.16554883000003],
            [907.982558, 142.29076883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#dc0c33",
          id_: "p89",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [907.982558, 142.29091883],
            [1235.380548, 142.29076883],
            [1235.380548, 661.07369883],
            [907.982558, 661.0738388300001]
          ],
          isRegion: true,
          stroke: "",
          fill: "#c200b3",
          id_: "p90",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [592.476648, 14.291108],
            [1235.380548, 14.291108],
            [1235.380548, 142.29109883],
            [592.476648, 142.29076883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#c2f0b3",
          id_: "p91",
          source: "",
          target: ""
        },
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
        },
        {
          graph: 1,
          points: [
            [293.386348, 661.0737688300001],
            [293.386348, 428.07354883],
            [315.906348, 428.07354883],
            [315.906348, 366.07376883],
            [127.060348, 366.07376883],
            [127.060348, 661.0737688300001]
          ],
          isRegion: true,
          stroke: "",
          fill: "#708a23",
          id_: "p84",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [315.906348, 14.41576883],
            [592.386348, 14.41576883],
            [592.386558, 256.57454882999997],
            [646.982558, 256.57454882999997],
            [646.982558, 428.07354883],
            [536.571828, 429.08590883],
            [426.320848, 428.07354883],
            [315.906348, 428.07354883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#5ac0ca",
          id_: "p85",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [17.3863510970132, 661.0737688300001],
            [17.3863510970132, 281.07376883],
            [315.247348, 281.07376883],
            [315.247348, 366.07376883],
            [127.060348, 366.07376883],
            [127.060348, 661.0737688300001]
          ],
          isRegion: true,
          stroke: "",
          fill: "#00ff00",
          id_: "p86",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [293.386348, 661.0737688300001],
            [530.558348, 661.0737688300001],
            [530.558348, 428.07354883],
            [293.386348, 428.07354883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#c2f013",
          id_: "p87",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [907.982558, 465.16554883000003],
            [907.982558, 661.0737688300001],
            [529.957608, 661.0737688300001],
            [530.150958, 428.07354883],
            [646.982558, 428.07354883],
            [646.982558, 465.16554883000003]
          ],
          isRegion: true,
          stroke: "",
          fill: "#3520ca",
          id_: "p88",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [592.386558, 142.29076883],
            [592.386348, 256.57454882999997],
            [646.982558, 256.57454882999997],
            [646.982558, 465.16554883000003],
            [907.982558, 465.16554883000003],
            [907.982558, 142.29076883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#dc0c33",
          id_: "p89",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [907.982558, 142.29091883],
            [1235.380548, 142.29076883],
            [1235.380548, 661.07369883],
            [907.982558, 661.0738388300001]
          ],
          isRegion: true,
          stroke: "",
          fill: "#c200b3",
          id_: "p90",
          source: "",
          target: ""
        },
        {
          graph: 1,
          points: [
            [592.476648, 14.291108],
            [1235.380548, 14.291108],
            [1235.380548, 142.29109883],
            [592.476648, 142.29076883]
          ],
          isRegion: true,
          stroke: "",
          fill: "#c2f0b3",
          id_: "p91",
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
        },
        {
          graph: 1,
          rId: "tspan4346-97",
          text: "Introductory",
          pos: [919.4891213999999, 57.95602013],
          fill: "#000000",
          align: "start"
        },
        {
          graph: 1,
          rId: "tspan4346-72",
          text: "Math and",
          pos: [214.9262414, 44.40537913],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4471",
          text: "Statistics",
          pos: [214.9262414, 68.15537913],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4471-4",
          text: "Humans and Computing",
          pos: [35.38368439999999, 344.76335213],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4344",
          text: "Numerical",
          pos: [402.0641114, 537.90224113],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346",
          text: "Computing",
          pos: [402.0641114, 561.65224113],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346-7",
          text: "Graphics",
          pos: [155.9642124, 394.92705113],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346-3",
          text: "Artificial Intelligence",
          pos: [700.2504214, 613.4540911299999],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346-5",
          text: "Software",
          pos: [661.4895263999999, 246.85242012999998],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4417",
          text: "Engineering",
          pos: [661.4895263999999, 270.60242013],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan3640",
          text: "Theory",
          pos: [369.8209414, 60.84219713],
          fill: "#000000",
          align: "start"
        },
        {
          graph: 1,
          rId: "tspan4346-9",
          text: "Systems",
          pos: [1088.1677413999998, 201.36453113000002],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346-97",
          text: "Introductory",
          pos: [919.4891213999999, 57.95602013],
          fill: "#000000",
          align: "start"
        },
        {
          graph: 1,
          rId: "tspan4346-72",
          text: "Math and",
          pos: [214.9262414, 44.40537913],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4471",
          text: "Statistics",
          pos: [214.9262414, 68.15537913],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4344",
          text: "Numerical",
          pos: [402.0641114, 537.90224113],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346",
          text: "Computing",
          pos: [402.0641114, 561.65224113],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346-7",
          text: "Graphics",
          pos: [155.9642124, 394.92705113],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346-3",
          text: "Artificial Intelligence",
          pos: [700.2504214, 613.4540911299999],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4346-5",
          text: "Software",
          pos: [661.4895263999999, 246.85242012999998],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan4417",
          text: "Engineering",
          pos: [661.4895263999999, 270.60242013],
          fill: "#000000",
          align: "begin"
        },
        {
          graph: 1,
          rId: "tspan3640",
          text: "Theory",
          pos: [369.8209414, 60.84219713],
          fill: "#000000",
          align: "start"
        }
      ]
    };

    const component = shallow(<RegionGroup {...props} />);
    expect(component).toMatchSnapshot();
  });
});
