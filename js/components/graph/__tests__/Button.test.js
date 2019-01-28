import React from 'react';
import { shallow } from 'enzyme';

import Button from '../Button'

describe('Button',() => {
    it('should render correctly with no props', () => {
        const component = shallow(<Button/>);
        expect(component).toMatchSnapshot();
    });
});
