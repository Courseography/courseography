import { configure } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';
configure({ adapter: new Adapter() });  // enzyme
import cscData from './components/graph/__mocks__/cscData';
import fetchMock from 'fetch-mock';

fetchMock.get('http://localhost/get-json-data?graphName=Computer+Science', cscData);

document.body.innerHTML = `
<nav>
    <li>
        <a id="nav-export">Export</a>
    </li>
</nav>
<div id="react-graph" class="react-graph"></div>
<div id="fcecount"></div>`;
