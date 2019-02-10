import React from 'react';
import Node from './Node';

export default class NodeGroup extends React.Component {
    constructor(props) {
        super(props);
        this.reset = this.reset.bind(this);
        this.findRelationship = this.findRelationship.bind(this);
    }

    reset() {
        this.props.nodesJSON.forEach(nodeJSON => {
            var node = this.refs[nodeJSON.id_];
            var state = node.props.parents.length === 0 ? 'takeable' : 'inactive';
            node.setState({ status: state, selected: false });
            localStorage.setItem(node.props.JSON.id_, state);
        });

        this.props.hybridsJSON.forEach(hybridJSON => {
            var hybrid = this.refs[hybridJSON.id_];
            var state = hybrid.props.parents.length === 0 ? 'takeable' : 'inactive';
            hybrid.setState({ status: state, selected: false });
            localStorage.setItem(hybrid.props.JSON.id_, state);
        });
    }

    /**
     * Helper for hybrid computation
     * @param {string} course
     * @returns {Node}
     */
    findRelationship(course) {
        var nodes = this.props.nodesJSON;
        var node = nodes.find(n =>
            n.type_ === 'Node' &&
            n.text.some(textTag => textTag.text.includes(course)));
        return node;
    }

    render() {
        var svg = this.props.svg;
        var highlightedNodes = this.props.highlightedNodes;
        var nodes = this.props.nodesJSON;
        var hybridRelationships = [];
        return (
            <g id='nodes'>
                {this.props.hybridsJSON.map(entry => {
                    var childs = [];
                    var outEdges = [];
                    this.props.edgesJSON.map(element => {
                        // Note: hybrids shouldn't have any in edges
                        if (entry.id_ === element.source) {
                            childs.push(element.target);
                            outEdges.push(element.id_);
                        }
                    });
                    // parse prereqs based on text
                    var hybridText = '';
                    entry.text.forEach(textTag => hybridText += textTag.text);
                    var parents = [];
                    // First search for entire string (see Stats graph)
                    var prereqNode = this.findRelationship(hybridText);
                    if (prereqNode !== undefined) {
                        parents.push(prereqNode.id_);
                        hybridRelationships.push([prereqNode.id_, entry.id_]);
                    } else { // Parse text first
                        var prereqs = parseAnd(hybridText)[0];
                        prereqs.forEach(course => {
                            if (typeof course === 'string') {
                                prereqNode = this.findRelationship(course);
                                if (prereqNode !== undefined) {
                                    parents.push(prereqNode.id_);
                                    hybridRelationships.push([prereqNode.id_, entry.id_]);
                                } else {
                                    console.error('Could not find prereq for ', hybridText);
                                }
                            } else if (typeof course === 'object') {
                                var orPrereq = [];
                                course.forEach(c => {
                                    var prereqNode = this.findRelationship(c);
                                    if (prereqNode !== undefined) {
                                        orPrereq.push(prereqNode.id_);
                                        hybridRelationships.push([prereqNode.id_, entry.id_]);
                                    } else {
                                        console.error('Could not find prereq for ', hybridText);
                                    }
                                });
                                if (orPrereq.length > 0) {
                                    parents.push(orPrereq);
                                }
                            }
                        });
                    }
                    return (
                      <Node
                        JSON={entry}
                        className={"hybrid"}
                        keynodesJSON={entry.id_}
                        hybrid={true}
                        ref={entry.id_}
                        parents={parents}
                        childs={childs}
                        inEdges={[]}
                        outEdges={outEdges}
                        svg={svg}
                        logicalType={"AND"}
                      />
                    );
                })}
                {this.props.nodesJSON.map((entry, value) => {
                    var highlighted = highlightedNodes.indexOf(entry.id_) >= 0;
                    var parents = [];
                    var childs = [];
                    var outEdges = [];
                    var inEdges = [];
                    this.props.edgesJSON.forEach((element, key) => {
                        if (entry.id_ === element.target) {
                            parents.push(element.source);
                            inEdges.push(element.id_);
                        } else if (entry.id_ === element.source) {
                            childs.push(element.target);
                            outEdges.push(element.id_);
                        }
                    });
                    hybridRelationships.forEach((element, key) => {
                        if (element[0] === entry.id_) {
                            childs.push(element[1]);
                        }
                    });
                    return <Node
                        JSON={entry}
                        className='node'
                        key={entry.id_}
                        ref={entry.id_}
                        hybrid={false}
                        parents={parents}
                        childs={childs}
                        inEdges={inEdges}
                        outEdges={outEdges}
                        svg={svg}
                        highlighted={highlighted}
                        onClick={this.props.nodeClick}
                        onMouseEnter={this.props.nodeMouseEnter}
                        onMouseLeave={this.props.nodeMouseLeave}
                        onMouseDown={this.props.nodeMouseDown}
                        onDraw={this.props.onDraw} />
                })}
            </g>
        );
    }
}

/**
 *
 * @param {string} s
 * @returns {Array}
 */
function parseAnd(s) {
    'use strict';

    var curr = s;
    var andList = [];
    while (curr.length > 0) {
        if (curr.charAt(0) === ',' ||
            curr.charAt(0) === ';' ||
            curr.charAt(0) === ' ') {
            curr = curr.substr(1);
        } else {
            var result = parseOr(curr);
            if (curr === result[1]) {
                console.error('Parsing failed for ' + s + '  with curr = ' + curr);
                break;
            } else {
                curr = result[1];
                andList.push(result[0]);
            }
        }
    }
    return [andList, curr];
}


/**
 *
 * @param {string} s
 * @returns {Array}
 */
function parseOr(s) {
    'use strict';

    var curr = s;
    var orList = [];
    var tmp;
    var result;
    var coursePrefix;
    while (curr.length > 0 &&
        curr.charAt(0) !== ',' &&
        curr.charAt(0) !== ';') {

        if (curr.charAt(0) === '(') {
            tmp = curr.substr(1, curr.indexOf(')'));
            if (coursePrefix === undefined && tmp.length >= 6) {
                coursePrefix = tmp.substr(0, 3).toUpperCase();
            }
            result = parseCourse(tmp, coursePrefix);

            orList.append(result[0]);
            curr = curr.substr(curr.indexOf(')') + 1);
        } else if (curr.charAt(0) === ' ' ||
            curr.charAt(0) === '/') {
            curr = curr.substr(1);
        } else {
            if (coursePrefix === undefined && curr.length >= 6) {
                coursePrefix = curr.substr(0, 3).toUpperCase();
            }
            result = parseCourse(curr, coursePrefix);
            if (curr === result[1]) {
                console.error('Parsing failed for ' + s + ' with curr = ' + curr);
                break;
            }
            curr = result[1];
            orList.push(result[0]);
        }
    }

    if (orList.length === 1) {
        orList = orList[0];
    }

    return [orList, curr];
}

/**
 *
 * @param {string} s
 * @param {string} prefix
 * @returns {Array}
 */
function parseCourse(s, prefix) {
    'use strict';

    var start = s.search(/[,/]/);

    if (start === 3) {
        return [prefix + s.substr(0, start), s.substr(start)];
    } else if (start > 0) {
        return [s.substr(0, start).toUpperCase(), s.substr(start)];
    }

    if (s.length === 3) {
        return [prefix + s, ''];
    }

    return [s, ''];
}
