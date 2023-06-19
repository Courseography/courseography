import { configure } from "enzyme"
import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
configure({ adapter: new Adapter() }) // enzyme
import testData from "./components/graph/__mocks__/defaultTestData"
import testContainerData from "./components/graph/__mocks__/testContainerData"
import aaa100CourseInfo from "./components/graph/__mocks__/aaa100-course-info"
import focusData from "./components/graph/__mocks__/focusData"
import statisticsTestData from "./components/graph/__mocks__/statisticsTestData"
import fetchMock from "fetch-mock"

fetchMock.get("http://localhost/get-json-data?graphName=Computer+Science", testData)
fetchMock.get(
  "http://localhost/get-json-data?graphName=%28unofficial%29+Statistics",
  statisticsTestData
)
fetchMock.get("http://localhost/course?name=aaa100H1", aaa100CourseInfo)
fetchMock.get("/course?name=aaa100H1", aaa100CourseInfo)
fetchMock.get("/course?name=aaa100", aaa100CourseInfo)
fetchMock.get(/\/post\?code=[A-Z]{5}[0-9]{4}([A-Z]*)/, focusData)
fetchMock.get("/graphs", testContainerData)
