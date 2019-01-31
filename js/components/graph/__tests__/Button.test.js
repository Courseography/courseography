import React from 'react';
import { shallow } from 'enzyme';

import Button from '../Button'

describe('Button', () => {
    it('Empty Button group should just have an empty g tag', () => {
        const ButtonProps = { "divId": "reset-button", "text": "Reset", "disabled": true };
        const component = shallow(<Button {...ButtonProps} />);
        expect(component).toMatchSnapshot();
    });
});
