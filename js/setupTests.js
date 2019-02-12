import { configure } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';
configure({ adapter: new Adapter() });

document.body.innerHTML = `
<nav>
    <li>
        <a id="nav-export">Export</a>
    </li>
</nav>
<div id="react-graph" class="react-graph"></div>`;
