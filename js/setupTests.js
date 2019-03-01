import { configure } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';
configure({ adapter: new Adapter() });
import nock from 'nock';
import cscData from './components/graph/__mocks__/cscData';

global.fetch = require("node-fetch");

// when running with Jest, it asks for
// http://localhost/get-json-data?graphName=Computer+Science
nock('http://localhost/')
    .get('/get-json-data?graphName=Computer+Science')
    .reply(200, () => {
        return cscData;
    });

document.body.innerHTML = `
<nav>
    <li>
        <a id="nav-export">Export</a>
    </li>
</nav>
<div id="react-graph" class="react-graph"></div>
<div id="fcecount"></div>`;
