import React from 'react';
import { mount } from 'enzyme';

import Node from '../Node'

describe('Node', () => {
    it('Hybrid node', () => {
        const nodeProps = { 
            JSON: {
                fill: "#888888",
                graph: 1,
                height: 24,
                id_: "h46",
                pos: [18.168848, 471.586698],
                stroke: "",
                text: [{
                    align: "begin",
                    fill: "",
                    graph: 1,
                    pos: [20.753048200000002, 481.389408],
                    rId: "text454",
                    text: "CSC318/418/",
                    type_: "Hybrid",
                    width: 65.207497
                }, {
                        align: "begin",
                        fill: "",
                        graph: 1,
                        pos: [32.698848, 493.640408],
                        rId: "text456",
                        text: "301/384",
                        length: 2,
                        type_: "Hybrid",
                        width: 65.207497
                }],
                type_: "Hybrid",
                width: 65.207497
            },
            childs: ["csc404"],
            className: "hybrid",
            hybrid: true,
            inEdges: [],
            logicalType: "AND",
            outEdges: ["p32"],
            parents: [Array(4)],
            svg: {
                onKeyDown: jest.fn()
            }
        };
        const component = mount(<Node {...nodeProps} />);
        expect(component).toMatchSnapshot();
    });
});
