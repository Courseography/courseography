import React from 'react';
import { shallow } from 'enzyme';

import Button from '../Button'

describe('Button', () => {
    it('Reset button', () => {
        const ButtonProps = { "divId": "reset-button", "text": "Reset", "disabled": true };
        const component = shallow(<Button {...ButtonProps} />);
        expect(component).toMatchSnapshot();
    });
});
