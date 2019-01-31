import React from 'react';
import { shallow } from 'enzyme';

import Graph from '../Graph'

describe('Graph', () => {
    it('should render correctly with graph props', () => {
        const graphProps = {
            edit: false,
            initialDrawMode: "draw-node",
            initialOnDraw: false,
            start_blank: false
        }
        const component = shallow(<Graph {...graphProps} />);
        expect(component).toMatchSnapshot();
    });
});
