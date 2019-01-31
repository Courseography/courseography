import React from 'react';
import { shallow } from 'enzyme';

import EdgeGroup from '../EdgeGroup'

describe('EdgeGroup', () => {
    it('Empty EdgeGroup group should just have an empty g tag', () => {
        const EdgeGroupProps = { 
            edgesJSON: [],
            svg: null
        };
        const component = shallow(<EdgeGroup {...EdgeGroupProps} />);
        expect(component).toMatchSnapshot();
    });
});
