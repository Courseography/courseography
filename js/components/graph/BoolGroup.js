import Bool from "./Bool";
import React from 'react';

export default class BoolGroup extends React.Component {
    constructor(props) {
        super(props);
        this.reset = this.reset.bind(this);
        this.generateBool = this.generateBool.bind(this);
    }

    componentDidMount() {
        for (var boolJSON of this.props.boolsJSON) {
            var ref = boolJSON.id_;
            this.refs[ref].updateNode(this.props.svg);
        }
    }

    reset() {
        this.props.boolsJSON.forEach((boolJSON) => {
            var bool = this.refs[boolJSON.id_];
            bool.setState({ status: 'inactive' });
        });
    }

    // Generate data for a Bool node
    generateBool(boolJSON) {
        var parents = [];
        var childs = [];
        var outEdges = [];
        var inEdges = [];
        this.props.edgesJSON.map((edge) => {
            if (boolJSON.id_ === edge.target) {
                parents.push(edge.source);
                inEdges.push(edge.id_);
            } else if (boolJSON.id_ === edge.source) {
                childs.push(edge.target);
                outEdges.push(edge.id_);
            }
        });

        return <Bool
            JSON={boolJSON}
            className='bool'
            key={boolJSON.id_}
            ref={boolJSON.id_}
            parents={parents}
            childs={childs}
            inEdges={inEdges}
            outEdges={outEdges}
            logicalType={(boolJSON.text[0] && boolJSON.text[0].text) || 'and'}
            svg={this.props.svg} />
    }

    render() {
        return (
            <g id='bools'>
                {this.props.boolsJSON.map(this.generateBool)}
            </g>
        );
    }
}
