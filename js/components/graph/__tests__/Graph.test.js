import React from 'react';
import { shallow } from 'enzyme';

import Graph from '../Graph'

describe('Graph', () => {
    it('should render correctly with no props', () => {
        const component = shallow(<Graph />);
        expect(component).toMatchSnapshot();
    });
});
