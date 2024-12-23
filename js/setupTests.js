import {
  AllCommunityModule,
  ModuleRegistry,
  provideGlobalGridOptions,
} from "ag-grid-community"

// Register all community features
ModuleRegistry.registerModules([AllCommunityModule])

// Mark all grids as using legacy themes
provideGlobalGridOptions({ theme: "legacy" })

import testData from "./components/graph/__mocks__/defaultTestData"
import testContainerData from "./components/graph/__mocks__/testContainerData"
import aaa100CourseInfo from "./components/graph/__mocks__/aaa100-course-info"
import bbb100CourseInfo from "./components/graph/__mocks__/bbb100-course-info"
import ccc100CourseInfo from "./components/graph/__mocks__/ccc100-course-info"
import focusData from "./components/graph/__mocks__/focusData"
import statisticsTestData from "./components/graph/__mocks__/statisticsTestData"
import fetchMock from "fetch-mock"
import courseToResp from "./components/generate/__mocks__/sample_responses.json"
import { TextEncoder, TextDecoder } from "util"

// This is needed after upgrade to React v18
global.TextEncoder = TextEncoder
global.TextDecoder = TextDecoder

fetchMock.get("http://localhost/get-json-data?graphName=Computer+Science", testData)
fetchMock.get(
  "http://localhost/get-json-data?graphName=%28unofficial%29+Statistics",
  statisticsTestData
)
fetchMock.get("http://localhost/course?name=aaa100H1", aaa100CourseInfo)
fetchMock.get("/course?name=aaa100H1", aaa100CourseInfo)
fetchMock.get("/course?name=aaa100", aaa100CourseInfo)
fetchMock.get("/course?name=BBB100H1", bbb100CourseInfo)
fetchMock.get("/course?name=CCC100H1", ccc100CourseInfo)
fetchMock.get(/\/post\?code=[A-Z]{5}[0-9]{4}([A-Z]*)/, focusData)
fetchMock.get("/graphs", testContainerData)
fetchMock.put("/graph-generate", graphGenMocker)

function graphGenMocker(_, opts) {
  const courseInput = JSON.parse(opts.body)["courses"].join(", ").trim()
  let resp = courseToResp[courseInput]
  let responseStatus, responseBody
  if (resp === undefined) {
    responseStatus = 400
    responseBody = "Inputted course does not exist in the sample-responses JSON file!"
  } else {
    responseStatus = 200
    responseBody = JSON.stringify(resp)
  }
  return {
    status: responseStatus,
    body: responseBody,
  }
}
