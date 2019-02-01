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
    it('Pressing a course node where you have the prerequisites should ', () => {

    });

    it('Hovering over a course node should create an "info" box', () => {
        
    });

    it("Clicking and hovering over Boolean and Hybrid nodes should do nothing", () => {
        // create a boolean node
        // create a hybrid node

        // hover over them
        // click them
    });

    it("Pressing a course node without the prerequisites should turn the node's border red", () => {
        // set CSC148 to be true
        // set CSC108 to be false
        // arrows going out of CSC148 should now be black instead of grey
        // arrows going out of CSC148 should be dotted, not solid
    });

    it("A selected course prerequisites should turn the node's border red", () => {
        // set CSC148 to be true
        // set CSC108 to be false
        // set mouse hover

        // those without the 
        // it should not 
    });

    it('Node Hover: Insufficient Prerequisite includes a boolean node and a hybrid node', () => {
        // Select CSC310
    });
});
