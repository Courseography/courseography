import React from 'react';
import { shallow } from 'enzyme';

import InfoBox from '../InfoBox'

describe('InfoBox', () => {
    it('Empty InfoBox group should just have an empty g tag', () => {
        const infoBoxProps = { 
            onClick: null,
            onMouseDown: null,
            onMouseLeave: null
        };
        const component = shallow(<InfoBox {...infoBoxProps} />);
        expect(component).toMatchSnapshot();
    });
});
