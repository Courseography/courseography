import React from 'react';
import { refLookUp } from '../common/utils'

export default class Bool extends React.Component {
    constructor(props) {
        super(props);
        this.state = { status: 'inactive' };
        this.isSelected = this.isSelected.bind(this);
        this.arePrereqsSatisfied = this.arePrereqsSatisfied.bind(this);
        this.updateNode = this.updateNode.bind(this);
        this.focusPrereqs = this.focusPrereqs.bind(this);
        this.unfocusPrereqs = this.unfocusPrereqs.bind(this);
    }

    isSelected() {
        return this.state.status == 'active';
    }

    arePrereqsSatisfied() {
        var svg = this.props.svg;
        function isAllTrue(element) {
            return (
                svg.refs['nodes'].refs[element] ?
                    svg.refs['nodes'].refs[element].isSelected() :
                    svg.refs['bools'].refs[element].isSelected());
        }

        if (this.props.logicalType === 'and') {
            return this.props.parents.every(isAllTrue);
        } else if (this.props.logicalType === 'or') {
            return this.props.parents.some(isAllTrue);
        }
    }

    updateNode() {
        var svg = this.props.svg;
        var newState = this.arePrereqsSatisfied() ? 'active' : 'inactive';

        var boolId = this.props.JSON.id_;
        this.setState({ status: newState }, function () {
            localStorage.setItem(boolId, newState);
            this.props.childs.forEach(function (node) {
                var currentNode = refLookUp(node, svg);
                currentNode.updateNode(svg);
            });
            var allEdges = this.props.outEdges.concat(this.props.inEdges);
            allEdges.forEach(function (edge) {
                var currentEdge = svg.refs['edges'].refs[edge];
                currentEdge.updateStatus();
            });
        });
    }

    focusPrereqs() {
        var svg = this.props.svg;
        // Check if there are any missing prerequisites.
        if (this.state.status !== 'active') {
            this.setState({ status: 'missing' }, () => {
                this.props.inEdges.forEach(function (edge) {
                    var currentEdge = svg.refs['edges'].refs[edge];
                    var sourceNode = refLookUp(currentEdge.props.source, svg);
                    if (!sourceNode.isSelected()) {
                        currentEdge.setState({ status: 'missing' });
                    }
                });
                this.props.parents.forEach(function (node) {
                    var currentNode = refLookUp(node, svg);
                    currentNode.focusPrereqs();
                });
            });
        }
    }

    unfocusPrereqs() {
        var svg = this.props.svg;
        this.updateNode(svg);
        this.props.parents.forEach(function (node, i) {
            var currentNode = refLookUp(node, svg);
            currentNode.unfocusPrereqs(svg);
        });
    }

    render() {
        var ellipseAttrs = {
            cx: this.props.JSON.pos[0],
            cy: this.props.JSON.pos[1],
            rx: '9.8800001',
            ry: '7.3684001'
        };
        return (
            <g {... this.props.JSON}
                className={this.props.className + ' ' + this.state.status} >
                <ellipse {...ellipseAttrs} />
                {this.props.JSON.text.map(function (textTag, i) {
                    var textAttrs = {
                        x: ellipseAttrs.cx,
                        y: textTag.pos[1]
                    };
                    return (
                        <text {...textAttrs} key={i}>
                            {this.props.logicalType}
                        </text>);
                }.bind(this))}
            </g>
        );
    }
}
