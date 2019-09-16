import { configure } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';
configure({ adapter: new Adapter() });  // enzyme
import testData from './components/graph/__mocks__/testData';
import aaa100CourseInfo from './components/graph/__mocks__/aaa100-course-info';
import fetchMock from 'fetch-mock';

fetchMock.get('http://localhost/get-json-data?graphName=Computer+Science', testData);
fetchMock.get('http://localhost/course?name=aaa100H1', aaa100CourseInfo);
fetchMock.get('/course?name=aaa100H1', aaa100CourseInfo);
fetchMock.get('/course?name=aaa100', aaa100CourseInfo);

document.body.innerHTML = `
<nav>
    <li>
        <a id="nav-export">Export</a>
    </li>
</nav>
<div id="react-graph" class="react-graph"></div>
<div id="fcecount"></div>`;
