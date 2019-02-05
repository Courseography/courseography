import React from 'react';
import { shallow } from 'enzyme';

import NodeGroup from '../NodeGroup'

describe('NodeGroup',() => {

    it('Node Group', () => {
        const props = {
            edgesJSON: [],
            highlightedNodes: [],
            hybridsJSON: [],
            nodeClick: () => 0,
            nodeMouseDown: () => 0,
            nodeMouseEnter: () => 0,
            nodeMouseLeave: () => 0,
            nodesJSON: [],
            onDraw: false,
            svg: null
        };
        const component = shallow(<NodeGroup {...props} />);
        expect(component).toMatchSnapshot();
    });
});
