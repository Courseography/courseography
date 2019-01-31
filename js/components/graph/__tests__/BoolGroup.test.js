import React from 'react';
import { shallow } from 'enzyme';

import BoolGroup from '../BoolGroup'

describe('BoolGroup', () => {
    it('Empty bool group should just have an empty g tag', () => {
        const boolGroupProps = {
            boolsJSON: [],
            egesJSON: [],
            svg: null
        }
        const component = shallow(<BoolGroup {...boolGroupProps} />);
        expect(component).toMatchSnapshot();
    });
});
