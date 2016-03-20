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


function renderReactGraph() {
    'use strict';
    return ReactDOM.render(
        <Graph width={1195} height={650}/>,
        document.getElementById('react-graph')
    );
}

/**
 * Converts a NamedNodeMap of SVG attributes into a dictionary
 * of attribute name-value pairs.
 * @param {NamedNodeMap} svgAttributes
 */
function getAttributes(svgAttributes) {
    'use strict';

    var attrs = {};
    for (var i = 0; i < svgAttributes.length; i++) {
        var item = svgAttributes[i];
        // Will be hard-coding in className and textAnchor and markerEnd
        var ignoredAttributes = ['class', 'text-anchor', 'marker-end'];
        if (ignoredAttributes.indexOf(item.name) === -1) {
            attrs[item.name] = item.value;
        }
    }
    return attrs;
}

/**
 * Converts a style string to dictionary.
 * @param {String} styleString
 */
function getStyles(styleString) {
    'use strict';

    //Have to check if it is null since null can't be split.
    if (styleString === null || styleString === undefined) {
        return {};
    }

    var styles = {};
    styleString.split(';').forEach(function (key, value) {
        var parts = key.split(':');
        if (parts.length === 2) {
            styles[parts[0]] = parts[1];
        }
    });
    return styles;
}

function getNodes(mode) {
    'use strict';

    return $('#graph ' + mode).map(function (key, element) {
        var children = Array.prototype.map.call(element.children,
            function (child) {
                var attrs = getAttributes(child.attributes);
                return {
                    'attributes': attrs,
                    'style': getStyles(attrs['style']),
                    //innerHTML is just for text within the <text>
                    //there aren't anymore children since it was assumed only one level of children
                    'innerHTML': child.innerHTML
                };
            });
        var attrs = getAttributes(element.attributes);
        return {
            'id': element.id,
            'attributes': attrs,
            'style': getStyles(attrs['style']),
            'children': children
        };
    }).get();
}


var Graph = React.createClass({
    getInitialState: function () {
        return {
            labelsJSON: [],
            regionsJSON: [],
            nodesJSON: [],
            hybridsJSON: [],
            boolsJSON: [],
            edgesJSON: [],
            highlightedNodes: [],
            fceCount: 0
        };
    },

    componentDidMount: function () {
        this.getGraph();
    },

    getGraph: function (graphId) {
        if (graphId === undefined) {
            var urlSpecifiedGraph = getURLParameter('dept');

            // HACK: Temporary workaround for giving the statistics department a link to our graph.
            // Should be replaced with a more general solution.
            if (urlSpecifiedGraph === 'sta') {
                graphId = '2';
            } else if (urlSpecifiedGraph !== null) {
                graphId = '1';
            } else {
                graphId = getCookie('active-graph');
                if (graphId === '') {
                    graphId = '1';
                }
            }
        }

        $.ajax({
            dataType: 'json',
            url: 'get-json-data',
            data: {'gid': graphId},
            success: function (data) {
                setCookie('active-graph', graphId);
                var labelsList = [];
                var regionsList = [];
                var nodesList = [];
                var hybridsList = [];
                var boolsList = [];
                var edgesList = [];

                //data[0] is ["texts", [JSON]]
                data[0][1].forEach(function (entry) {
                    if (entry['rId'].substring(0,5) === 'tspan') {
                        labelsList.push(entry);
                    }
                });
                //data[1] is ["shapes", [JSON]]
                data[1][1].forEach(function (entry) {
                    if (entry['type_'] === 'Node') {
                        nodesList.push(entry);
                    } else
                    if (entry['type_'] === 'Hybrid') {
                        hybridsList.push(entry);
                    } else
                    if (entry['type_'] === 'BoolNode') {
                        boolsList.push(entry);
                    }
                });
                //data[2] is ["paths", [JSON]]
                //data[2][1] are the JSON without "paths"
                data[2][1].forEach(function (entry) {
                    if (entry['isRegion']) {
                        regionsList.push(entry);
                    } else {
                        edgesList.push(entry);
                    }
                });

                if (this.isMounted()) {
                    this.setState({labelsJSON: labelsList,
                                   regionsJSON: regionsList,
                                   nodesJSON: nodesList,
                                   hybridsJSON: hybridsList,
                                   boolsJSON: boolsList,
                                   edgesJSON: edgesList});
                }
            }.bind(this),
            error: function(xhr, status, err) {
                console.error('graph-json', status, err.toString());
            }
        });

        //Need to hardcode these in because React does not understand these attributes
        var svgNode = ReactDOM.findDOMNode(this.refs.svg);
        var markerNode = ReactDOM.findDOMNode(this.refs.marker);

        svgNode.setAttribute('xmlns','http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:xlink','http://www.w3.org/1999/xlink');
        svgNode.setAttribute('xmlns:svg','http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:dc','http://purl.org/dc/elements/1.1/');
        svgNode.setAttribute('xmlns:cc','http://creativecommons.org/ns#');
        svgNode.setAttribute('xmlns:rdf','http://www.w3.org/1999/02/22-rdf-syntax-ns#');

        markerNode.setAttribute('refX', 4);
        markerNode.setAttribute('refY', 5);
        markerNode.setAttribute('markerUnits', 'strokeWidth');
        markerNode.setAttribute('orient', 'auto');
        markerNode.setAttribute('markerWidth', 7);
        markerNode.setAttribute('markerHeight', 7);
    },

    componentDidUpdate: function (prevProps, prevState) {
        if (prevState.nodesJSON !== this.state.nodesJSON) {
            var totalFCEs = 0;
            for (var ref in this.refs.nodes.refs) {
                var node = this.refs.nodes.refs[ref];
                if (!node.props.hybrid && node.state.selected) {
                    totalFCEs += 0.5;
                }
            }
            this.setFCECount(totalFCEs);
        }
    },


    nodeClick: function (event) {
        var courseID = event.currentTarget.id;
        var currentNode = this.refs['nodes'].refs[courseID];
        var wasSelected = currentNode.state.selected;
        currentNode.toggleSelection(this);
        if (wasSelected) {
            // TODO: Differentiate half- and full-year courses
            this.incrementFCECount(-0.5);
        } else {
            this.incrementFCECount(0.5);
        }
    },

    nodeMouseEnter: function (event) {
        var courseID = event.currentTarget.id;
        var currentNode = this.refs['nodes'].refs[courseID];
        currentNode.focusPrereqs(this);

        // Old hover modal code
        if ($('.modal').length === 0) {
            $('.tooltip-group').remove();
            displayTooltip(courseID);
        }
    },

    nodeMouseLeave: function (event) {
        var courseID = event.currentTarget.id;
        var currentNode = this.refs['nodes'].refs[courseID];
        currentNode.unfocusPrereqs(this);

        // Old hover modal code
        if ($('.modal').length === 0) {
            removeToolTip(this);
        }
    },

    setFCECount: function (credits) {
        this.setState({fceCount: credits}, function () {
            $('#fcecount').text('FCE Count: ' + this.state.fceCount);
        });
    },

    incrementFCECount: function (credits) {
        this.setState({fceCount: this.state.fceCount + credits}, function () {
            $('#fcecount').text('FCE Count: ' + this.state.fceCount);
        });
    },

    // Reset graph
    reset: function () {
        this.setFCECount(0);
        this.refs.nodes.reset();
        this.refs.bools.reset();
        this.refs.edges.reset();
    },

    render: function () {
        //not all of these properties are supported in React
        var svgAttrs = {'width': this.props.width, 'height': this.props.height};
        var markerAttrs = {'id': 'arrowHead'};
        var polylineAttrs = {'points': '0,1 10,5 0,9', 'fill': 'black'};
        return (
            <svg {... svgAttrs} ref='svg' version='1.1'
                 className={this.state.highlightedNodes.length > 0 ?
                            'highlight-nodes' : ''}>
                <defs>
                    <marker {... markerAttrs} ref='marker'
                            viewBox='0 0 10 10'>
                        <polyline {... polylineAttrs}/>
                    </marker>
                </defs>
                <RegionGroup ref='regions' regionsJSON={this.state.regionsJSON}/>
                <NodeGroup ref='nodes'
                            onClick={this.nodeClick}
                            nodeMouseEnter={this.nodeMouseEnter}
                            nodeMouseLeave={this.nodeMouseLeave}
                            svg={this}
                            nodesJSON={this.state.nodesJSON}
                            hybridsJSON={this.state.hybridsJSON}
                            edgesJSON={this.state.edgesJSON}
                            highlightedNodes={this.state.highlightedNodes}/>
                <BoolGroup ref='bools' boolsJSON={this.state.boolsJSON} edgesJSON={this.state.edgesJSON} svg={this}/>
                <EdgeGroup svg={this} ref='edges' edgesJSON={this.state.edgesJSON}/>
                <RegionLabelGroup ref='regionLabels' labelsJSON={this.state.labelsJSON}/>
            </svg>
        );
    }
});


var RegionLabelGroup = React.createClass({
    render: function () {
        return (
            <g id='region-labels'>
                {this.props.labelsJSON.map(function (entry, value) {
                    var textAttrs = {};
                    textAttrs['x'] = entry.pos[0];
                    textAttrs['y'] = entry.pos[1];

                    var textStyle = {
                        fill : entry['fill']
                    }

                    return <text
                            {... textAttrs}
                            key={value}
                            style={textStyle}
                            textAnchor={entry['text-anchor']}>
                                {entry['text']}
                           </text>
                })}
            </g>
        );
    }
});

var RegionGroup = React.createClass({
    getInitialState: function () {
        return {
            regionsList: []
        };
    },

    componentDidMount: function () {
        this.parseSVG();
    },

    parseSVG: function () {
        this.setState({regionsList: getNodes('.region')});
    },

    render: function () {
        return (
            <g id='regions'>
                {this.props.regionsJSON.map(function (entry, value) {
                    var pathAttrs = {};
                    pathAttrs['d'] = 'M';

                    entry.points.forEach(function(x){
                        pathAttrs['d'] += x[0] + ',' + x[1] + ' '});

                    var pathStyle = {
                        fill : entry.fill
                    }
                    return <path {... pathAttrs} key={value} className='region' style={pathStyle}>
            </path>
                })}
            </g>
        );
    }
});


// This now uses the new syntax for a stateless React component
// (component with only a render method).
// It also uses ES2015 "fat arrow" syntax for function definition.
var Region = ({attributes, styles}) => {
    return (
        <path {... attributes}
              className='region'
              style={styles} />
    );
};


var NodeGroup = React.createClass({
    getInitialState: function () {
        return {
            nodesList: [],
            hybridsList: []
        };
    },

    componentDidMount: function () {
        this.parseSVG();
    },

    parseSVG: function () {
        this.setState({nodesList: getNodes('.node'), hybridsList: getNodes('.hybrid')});
    },

    reset: function () {
        this.props.nodesJSON.forEach((nodeJSON) => {
            var node = this.refs[nodeJSON.id_];
            var state = node.props.parents.length === 0 ? 'takeable' : 'inactive';
            node.setState({status: state, selected: false});
            setCookie(node.props.JSON.id_, state);
        });

        this.props.hybridsJSON.forEach((hybridJSON) => {
            var hybrid = this.refs[hybridJSON.id_];
            var state = hybrid.props.parents.length === 0 ? 'takeable' : 'inactive';
            hybrid.setState({status: state, selected: false});
            setCookie(hybrid.props.JSON.id_, state);
        })
    },

    render: function () {
        var svg = this.props.svg;
        var highlightedNodes = this.props.highlightedNodes;
        var nodes = this.props.nodesJSON;
        var hybridRelationships = [];
        return (
            <g id='nodes' stroke='black'>
                {this.props.hybridsJSON.map(function (entry, value) {
                    var childs = [];
                    var outEdges = [];
                    this.props.edgesJSON.map(function (element, key) {
                        // Note: hybrids shouldn't have any in edges
                        if (entry.id_ === element.source) {
                            childs.push(element.target);
                            outEdges.push(element.id_);
                        }
                    });
                    // parse prereqs based on text
                    var hybridText = "";
                    entry.text.forEach(function (textTag, i) {
                        hybridText += textTag.text;
                    });
                    var prereqs = parseAnd(hybridText)[0];
                    var parents = [];
                    prereqs.forEach(function (course, i) {
                        if (typeof(course) === 'string') {
                            var prereqNode = null;
                            for (var i = 0; i < nodes.length; i++) {
                                for (var j = 0; j < nodes[i].text.length; j++) {
                                    if (nodes[i].text[j].text.includes(course)) {
                                        prereqNode = nodes[i];
                                        break;
                                    }
                                }
                                if (prereqNode !== null) {
                                    break;
                                }
                            }
                            if (prereqNode !== null) {
                                parents.push(nodes[i].id_);
                                hybridRelationships.push([nodes[i].id_, entry.id_]);
                            }
                        } else if (typeof(course) === 'object') {
                            var orPrereq = [];
                            for (var k = 0; k < course.length; k++) {
                                var prereqNode = null;
                                var i;
                                for (i = 0; i < nodes.length; i++) {
                                    for (var j = 0; j < nodes[i].text.length; j++) {
                                        if (nodes[i].text[j].text.includes(course[k])) {
                                            prereqNode = nodes[i];
                                            break;
                                        }
                                    }
                                    if (prereqNode !== null) {
                                        break;
                                    }
                                }
                                if (prereqNode !== null) {
                                    orPrereq.push(prereqNode.id_);
                                    hybridRelationships.push([prereqNode.id_, entry.id_]);
                                }

                            }
                            if (orPrereq.length > 0) {
                                parents.push(orPrereq);
                            }
                        }
                    });
                    return <Node
                            JSON={entry}
                            className={'hybrid'}
                            key={entry.id_}
                            hybrid={true}
                            ref={entry.id_}
                            parents={parents}
                            childs={childs}
                            inEdges={[]}
                            outEdges={outEdges}
                            svg={svg}
                            logicalType={'AND'}/>
                }, this)}
                {this.props.nodesJSON.map(function (entry, value) {
                    var highlighted = highlightedNodes.indexOf(entry.id_) >= 0;
                    var parents = [];
                    var childs = [];
                    var outEdges = [];
                    var inEdges = [];
                    this.props.edgesJSON.forEach(function (element, key) {
                        if (entry.id_ === element.target) {
                            parents.push(element.source);
                            inEdges.push(element.id_);
                        } else if (entry.id_ === element.source) {
                            childs.push(element.target);
                            outEdges.push(element.id_);
                        }
                    });
                    hybridRelationships.forEach(function (element, key) {
                        if (element[0] === entry.id_) {
                            childs.push(element[1]);
                        }
                    });
                    return <Node
                            JSON={entry}
                            className={'node'}
                            key={entry.id_}
                            hybrid={false}
                            ref={entry.id_}
                            parents={parents}
                            childs={childs}
                            inEdges={inEdges}
                            outEdges={outEdges}
                            {... this.props}
                            svg={svg}
                            logicalType={'AND'}
                            highlighted={highlighted}
                            onMouseEnter={this.props.nodeMouseEnter}
                            onMouseLeave={this.props.nodeMouseLeave} />
                }, this)}
            </g>
        );
    }
});


var Node = React.createClass({
    getInitialState: function () {
        var state = getCookie(this.props.JSON.id_);
        if (state === '') {
            state = this.props.parents.length === 0 ? 'takeable' : 'inactive';
        }
        return {
            status: state,
            selected: ['active', 'overridden'].indexOf(state) >= 0
        };
    },

    isSelected: function () {
        if (this.props.hybrid) {
            return this.state.status === 'active';
        } else {
            return this.state.selected;
        }
    },

    arePrereqsSatisfied: function (svg) {
        function isAllTrue(element) {
            if (typeof(element) === 'string') {
                return (svg.refs['nodes'].refs[element] ?
                        svg.refs['nodes'].refs[element].isSelected() :
                        svg.refs['bools'].refs[element].isSelected());
            } else {
                return element.some(isAllTrue);
            }
        }

        if (this.props.logicalType === 'AND') {
            return this.props.parents.every(isAllTrue);
        } else if (this.props.logicalType === 'OR') {
            return this.props.parents.some(isAllTrue);
        }
    },

    updateNode: function (svg, recursive) {
        var newState;
        if (this.arePrereqsSatisfied(svg)) {
            if (this.isSelected() || this.props.hybrid) {
                newState = 'active';
            } else {
                newState = 'takeable';
            }
        } else {
            if (this.isSelected() && !this.props.hybrid) {
                newState = 'overridden';
            } else {
                newState = 'inactive';
            }
        }

        var nodeId = this.props.JSON.id_;

        // Check whether need to update children
        if ((['active', 'overridden'].indexOf(newState) >= 0) ===
            (['active', 'overridden'].indexOf(this.state.status) >= 0) &&
            this.state.status !== 'missing') {
            setCookie(nodeId, newState);
            this.setState({status: newState});
            return;
        }

        if (recursive === undefined || recursive) {
            this.setState({status: newState}, function () {
                setCookie(nodeId, newState);
                this.props.childs.forEach(function (node) {
                    var currentNode = svg.refs['nodes'].refs[node] ||
                                      svg.refs['bools'].refs[node];
                    currentNode.updateNode(svg);
                });
                var allEdges = this.props.outEdges.concat(this.props.inEdges);
                allEdges.forEach(function (edge) {
                    var currentEdge = svg.refs['edges'].refs[edge];
                    currentEdge.updateEdge(svg);
                }.bind(this));
            });
        } else {
            this.setState({status: newState});
            setCookie(nodeId, newState);
        }
    },

    toggleSelection: function (svg) {
        this.setState({selected: !this.state.selected}, function () {
            this.updateNode(svg);
        })
    },

    focusPrereqs: function (svg) {
        // Check if there are any missing prerequisites.
        var id = this.props.JSON.id_;
        if (['inactive', 'overridden', 'takeable'].indexOf(this.state.status) >= 0) {
            this.setState({status: 'missing'}, function () {
                this.props.inEdges.forEach(function (edge) {
                    var currentEdge = svg.refs['edges'].refs[edge];
                    var sourceNode = svg.refs['nodes'].refs[currentEdge.props.source] ||
                                     svg.refs['bools'].refs[currentEdge.props.source];
                    if (!sourceNode.isSelected()) {
                        currentEdge.setState({status: 'missing'});
                    }
                });
                var isHybrid = this.props.hybrid;
                this.props.parents.forEach(function (node) {
                    var currentNode = svg.refs['nodes'].refs[node] ||
                                      svg.refs['bools'].refs[node];
                    currentNode.focusPrereqs(svg);
                });
            });
        }
    },

    unfocusPrereqs: function (svg) {
        this.updateNode(svg, false);
        this.props.parents.forEach(function (node) {
            var currentNode = svg.refs['nodes'].refs[node] ||
                              svg.refs['bools'].refs[node];
            currentNode.unfocusPrereqs(svg);
        });
        this.props.inEdges.forEach(function (edge) {
            var currentEdge = svg.refs['edges'].refs[edge];
            if (currentEdge.state.status === 'missing') {
                currentEdge.updateEdge(svg);
            }
        });
    },

    render: function () {
        var newClassName = this.props.className + ' ' + this.state.status;
        if (this.props.highlighted) {
            var attrs = this.props.JSON;
            var width = parseFloat(attrs.width) / 2;
            var height = parseFloat(attrs.height) / 2;
            var cx = parseFloat(attrs.pos[0]) + width;
            var cy = parseFloat(attrs.pos[1]) + height;
            var rx = width + 9;
            var ry = height + 8.5;
            var ellipse = (
                <ellipse
                    className='spotlight'
                    cx={cx}
                    cy={cy}
                    rx={rx}
                    ry={ry} />
                );
        } else {
            var ellipse = null;
        }

        var gAttrs = {};
        gAttrs['text-rendering'] = 'geometricPrecision';
        gAttrs['shape-rendering'] = 'geometricPrecision';

        var rectAttrs = {};
        rectAttrs['height'] = this.props.JSON.height;
        rectAttrs['width'] = this.props.JSON.width;
        rectAttrs['rx'] = '4';
        rectAttrs['ry'] = '4';
        rectAttrs['x'] = this.props.JSON.pos[0];
        rectAttrs['y'] = this.props.JSON.pos[1];

        var rectStyle = {
            fill : this.props.JSON.fill
        }

        var textXOffset = this.props.JSON.pos[0] + this.props.JSON.width / 2;

        return (
            <g {... this.props}{... gAttrs} id={this.props.JSON.id_}
               className={newClassName} >
                {ellipse}
                <rect {... rectAttrs} style={rectStyle} />
                {this.props.JSON.text.map(function (textTag, value) {
                    var textAttrs = {};
                    textAttrs['x'] = textXOffset;
                    textAttrs['y'] = textTag.pos[1];
                    return (
                        <text {... textAttrs}
                            key={textTag.rId}>
                            {textTag.text}
                        </text>);
                })}
            </g>
        );
    }
});


var BoolGroup = React.createClass({
    getInitialState: function () {
        return {
            boolsList: []
        };
    },

    componentDidMount: function () {
        this.parseSVG();
        for (var ref in this.refs) {
            this.refs[ref].updateNode(this.props.svg);
        }
    },

    parseSVG: function () {
        this.setState({boolsList: getNodes('.bool')});
    },

    reset: function () {
        this.props.boolsJSON.forEach((boolJSON) => {
            var bool = this.refs[boolJSON.id_];
            bool.setState({status: 'inactive'});
        });
    },

    render: function () {
        var svg = this.props.svg;
        return (
            <g id='bools'>
                {this.props.boolsJSON.map(function (entry, value) {
                    var parents = [];
                    var childs = [];
                    var outEdges = [];
                    var inEdges = [];
                    this.props.edgesJSON.map(function (element, key) {
                        if (entry.id_ === element.target) {
                            parents.push(element.source);
                            inEdges.push(element.id_);
                        }
                        if (entry.id_ === element.source) {
                            childs.push(element.target);
                            outEdges.push(element.id_);
                        }
                    });
                    return <Bool
                            JSON={entry}
                            className='bool'
                            key={entry.id_}
                            ref={entry.id_}
                            parents={parents}
                            childs={childs}
                            inEdges={inEdges}
                            outEdges={outEdges}
                            hybrid={true}
                            logicalType={entry.text[0].text}
                            svg={svg} />
                }, this)}
            </g>
        );
    }
});


var Bool = React.createClass({
    getInitialState: function () {
        return {status: 'inactive'};
    },

    isSelected: function () {
        return this.state.status == 'active';
    },

    arePrereqsSatisfied: function (svg) {
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
    },

    updateNode: function (svg) {
        var newState;
        if (this.arePrereqsSatisfied(svg)) {
            newState = 'active';
        } else {
            newState = 'inactive';
        }

        var boolId = this.props.JSON.id_;
        this.setState({status: newState}, function () {
            setCookie(boolId, newState);
            this.props.childs.forEach(function (node) {
                var currentNode = svg.refs['nodes'].refs[node] ||
                                  svg.refs['bools'].refs[node];
                currentNode.updateNode(svg);
            });
            var allEdges = this.props.outEdges.concat(this.props.inEdges);
            allEdges.forEach(function (edge) {
                var currentEdge = svg.refs['edges'].refs[edge];
                currentEdge.updateEdge(svg);
            });
        });
    },

    focusPrereqs: function (svg) {
        // Check if there are any missing prerequisites.
        if (this.state.status !== 'active') {
            this.setState({status: 'missing'}, function () {
                this.props.inEdges.forEach(function (edge) {
                    var currentEdge = svg.refs['edges'].refs[edge];
                    var sourceNode = svg.refs['nodes'].refs[currentEdge.props.source] ||
                                     svg.refs['bools'].refs[currentEdge.props.source];
                    if (!sourceNode.isSelected()) {
                        currentEdge.setState({status: 'missing'});
                    }
                });
                this.props.parents.forEach(function (node) {
                    var currentNode = svg.refs['nodes'].refs[node] ||
                                      svg.refs['bools'].refs[node];
                    currentNode.focusPrereqs(svg);
                });
            });
        }
    },

    unfocusPrereqs: function (svg) {
        this.updateNode(svg);
        this.props.parents.forEach(function (node, i) {
            var currentNode = svg.refs['nodes'].refs[node] ||
                              svg.refs['bools'].refs[node];
            currentNode.unfocusPrereqs(svg);
        });
    },

    render: function () {
        var ellipseAttrs = {};
        ellipseAttrs['cx'] = this.props.JSON.pos[0];
        ellipseAttrs['cy'] = this.props.JSON.pos[1];
        ellipseAttrs['rx'] = '9.8800001';
        ellipseAttrs['ry'] = '7.3684001';
        return (
            <g {... this.props.JSON}
               className={this.props.className + ' ' + this.state.status} >
                <ellipse {... ellipseAttrs}/>
                {this.props.JSON.text.map(function (textTag, value) {
                    var textAttrs = {};
                    textAttrs['x'] = ellipseAttrs['cx'];
                    textAttrs['y'] = textTag.pos[1];
                    return (
                        <text {... textAttrs}
                              key={value}
                              textAnchor='middle'
                              stroke='none'>
                            {this.props.logicalType}
                        </text>);
                }.bind(this))}
            </g>
        );
    }
});


var EdgeGroup = React.createClass({
    getInitialState: function () {
        return {
            edgesList: []
        };
    },

    componentDidMount: function () {
        this.parseSVG();
        for (var ref in this.refs) {
            this.refs[ref].updateEdge(this.props.svg);
        }
    },

    parseSVG: function () {
        this.setState({edgesList: getNodes('.path')});
    },

    reset: function () {
        this.props.edgesJSON.forEach((edgeJSON) => {
            var edge = this.refs[edgeJSON.id_];
            edge.setState({status: 'inactive'});
        });
    },

    render: function () {
        var svg = this.props.svg;
        return (
            <g id='edges' stroke='black'>
                {this.props.edgesJSON.map(function (entry, value) {
                    return <Edge
                            className='path'
                            key={entry.id_}
                            ref={entry.id_}
                            source={entry.source}
                            target={entry.target}
                            JSON={entry}
                            svg={svg}/>
                })}
            </g>
        );
    }
});


var Edge = React.createClass({
    getInitialState: function () {
        return {status: 'inactive'};
    },

    updateEdge: function (svg) {
        var sourceNode = svg.refs['nodes'].refs[this.props.source] ||
                         svg.refs['bools'].refs[this.props.source];
        var targetNode = svg.refs['nodes'].refs[this.props.target] ||
                         svg.refs['bools'].refs[this.props.target];
        if (!sourceNode.isSelected()) {
            this.setState({status: 'inactive'});
        } else if (!targetNode.isSelected()) {
            this.setState({status: 'takeable'});
        } else {
            this.setState({status: 'active'});
        }
    },

    render: function () {
        var pathAttrs = {};
        pathAttrs['d'] = 'M';
        this.props.JSON.points.forEach(function(x){
            pathAttrs['d'] += x[0] + ',' + x[1] + ' '});

        return (
            <path {... pathAttrs}
                  className={this.props.className + ' ' + this.state.status}
                  markerEnd='url(#arrowHead)'>
            </path>
        );
    }
});

export default {renderReactGraph: renderReactGraph};
