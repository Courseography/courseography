import React from 'react';
import { shallow } from 'enzyme';

import Edge from '../Edge'

describe('Edge',() => {
    it('1 + 1 should be 2', () => {
        expect(1).toBe(2);
    });
    it('should render correctly with no props', () => {
        const component = shallow(<Edge points={[[1, 1], [2, 2]]}/>);
        expect(component).toMatchSnapshot();
    });
});
