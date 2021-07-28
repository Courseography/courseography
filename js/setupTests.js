import { configure } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';
configure({ adapter: new Adapter() });  // enzyme
import testData from './components/graph/__mocks__/defaultTestData';
import testContainerData from './components/graph/__mocks__/testContainerData';
import aaa100CourseInfo from './components/graph/__mocks__/aaa100-course-info';
import statisticsTestData from './components/graph/__mocks__/statisticsTestData';
import fetchMock from 'fetch-mock';
import $ from '../public/js/vendor/jquery.min.1.10.2';

// Need to import jQuery globally as we are loading it through Haskell
global.$ = global.jQuery = $;

fetchMock.get('http://localhost/get-json-data?graphName=Computer+Science', testData);
fetchMock.get('http://localhost/get-json-data?graphName=%28unofficial%29+Statistics', statisticsTestData);
fetchMock.get('http://localhost/course?name=aaa100H1', aaa100CourseInfo);
fetchMock.get('/course?name=aaa100H1', aaa100CourseInfo);
fetchMock.get('/course?name=aaa100', aaa100CourseInfo);
fetchMock.get('/graphs', testContainerData);

document.body.innerHTML = `
<nav>
    <ul>
        <li>
            <a id="nav-graph" href="/graph">Graph</a>
        </li>
    </ul>
</nav>
<div id="react-graph" class="react-graph"></div>
<div id="fcecount"></div>`;
