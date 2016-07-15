import * as tooltip from 'es6!graph/tooltip';
import {Modal} from 'es6!common/react_modal';

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


function Button(props) {
    return (
        <button id={props.divId} className='graph-control-button'
        onMouseDown={props.mouseDown}
        onMouseUp={props.mouseUp}
        disabled={props.disabled}>{props.text}</button>
    );
}


function renderReactGraph() {
    'use strict';
    return ReactDOM.render(
        <Graph/>,
        document.getElementById('react-graph')
    );
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
            timeouts: [],
            fceCount: 0,
            width: 0,
            height: 0,
            zoomFactor: 1,
            horizontalPanFactor: 0,
            verticalPanFactor: 0,
            mouseDown: false
        };
    },

    componentDidMount: function () {
        this.getGraph();
        // can't detect keydown event when adding event listener to react-graph
        document.body.addEventListener('keydown', this.onKeyDown);
        document.getElementById('react-graph').addEventListener('wheel', this.onWheel);
    },

    componentWillUnmount: function () {
        document.body.removeEventListener('keydown', this.onKeyDown);
        document.getElementById('react-graph').removeEventListener('wheel', this.onWheel);
    },

    getGraph: function (graphName) {
        if (graphName === undefined) {
            var urlSpecifiedGraph = getURLParameter('dept');

            // HACK: Temporary workaround for giving the statistics department a
            // link to our graph.
            // Should be replaced with a more general solution.
            if (urlSpecifiedGraph === 'sta') {
                graphName = 'Statistics';
            } else if (urlSpecifiedGraph !== null) {
                graphName = 'Computer Science';
            } else {
                graphName = getCookie('active-graph');
                if (graphName === '') {
                    graphName = 'Computer Science';
                }
            }
        }

        graphName = graphName.replace('-', ' ');

        $.ajax({
            dataType: 'json',
            url: 'get-json-data',
            data: {'graphName': graphName},
            success: function (data) {
                setCookie('active-graph', graphName);
                var regionsList = [];
                var nodesList = [];
                var hybridsList = [];
                var boolsList = [];
                var edgesList = [];

                var labelsList = data.texts.filter(function (entry) {
                    return entry.rId.startsWith('tspan');
                });

                data.shapes.forEach(function (entry) {
                    if (entry.type_ === 'Node') {
                        nodesList.push(entry);
                    } else if (entry.type_ === 'Hybrid') {
                        hybridsList.push(entry);
                    } else if (entry.type_ === 'BoolNode') {
                        boolsList.push(entry);
                    }
                });

                data.paths.forEach(function (entry) {
                    if (entry.isRegion) {
                        regionsList.push(entry);
                    } else {
                        edgesList.push(entry);
                    }
                });

                if (this.isMounted()) {
                    this.setState({
                        labelsJSON: labelsList,
                        regionsJSON: regionsList,
                        nodesJSON: nodesList,
                        hybridsJSON: hybridsList,
                        boolsJSON: boolsList,
                        edgesJSON: edgesList,
                        width: data.width,
                        height: data.height,
                        zoomFactor: 1,
                        horizontalPanFactor: 0,
                        verticalPanFactor: 0
                    });
                }
            }.bind(this),
            error: function (xhr, status, err) {
                console.error('graph-json', status, err.toString());
            }
        });

        // Need to hardcode these in because React does not understand these
        // attributes
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

    clearAllTimeouts: function () {
        for (var i = 0; i < this.state.timeouts.length; i++) {
            clearTimeout(this.state.timeouts[i]);
        }

        this.setState({timeouts: []});
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

    nodeClick: function (event) {
        var courseId = event.currentTarget.id;
        var currentNode = this.refs.nodes.refs[courseId];
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
        var courseId = event.currentTarget.id;
        var currentNode = this.refs.nodes.refs[courseId];
        currentNode.focusPrereqs(this);

        this.clearAllTimeouts();

        var infoBox = this.refs.infoBox;

        var xPos = currentNode.props.JSON.pos[0];
        var yPos = currentNode.props.JSON.pos[1];
        var rightSide = xPos > 222;
        // The tooltip is offset with a 'padding' of 5.
        if (rightSide) {
                xPos = parseFloat(xPos) - 65;
        } else {
                xPos = parseFloat(xPos) +
                       parseFloat(currentNode.props.JSON.width) + 5;
        }

        yPos = parseFloat(yPos);

        infoBox.setState({xPos: xPos,
                          yPos: yPos,
                          nodeId: courseId,
                          showInfobox: true});
    },

    nodeMouseLeave: function (event) {
        var courseId = event.currentTarget.id;
        var currentNode = this.refs.nodes.refs[courseId];
        currentNode.unfocusPrereqs(this);

        var infoBox = this.refs.infoBox;

        var timeout = setTimeout(function () {
                infoBox.setState({showInfobox: false});
        }, 400);


        this.setState({timeouts: this.state.timeouts.concat(timeout)});

    },

    infoBoxMouseEnter: function () {
        this.clearAllTimeouts();

        var infoBox = this.refs.infoBox;
        infoBox.setState({showInfobox: true});
    },

    infoBoxMouseLeave: function () {
        var infoBox = this.refs.infoBox;

        var timeout = setTimeout(function () {
                infoBox.setState({showInfobox: false});
        }, 400);

        this.setState({timeouts: this.state.timeouts.concat(timeout)});
    },

    infoBoxMouseClick: function () {
        var infoBox = this.refs.infoBox;
        var modal = this.refs.modal;
        modal.setState({courseId: infoBox.state.nodeId.substring(0, 6)}, function (){
            $.ajax({
                url: 'course',
                data: {name: formatCourseName(modal.state.courseId)[0]},
                dataType: 'json',
                success: function (data) {
                    if (modal.isMounted()) {
                        //This is getting the session times
                        var sessions = data.fallSession.lectures
                                                       .concat(data.springSession.lectures)
                                                       .concat(data.yearSession.lectures)
                        //Tutorials don't have a timeStr to print, so I've currently omitted them
                        modal.setState({course: data, sessions: sessions});
                    }
                },
                error: function (xhr, status, err) {
                    console.error('course-info', status, err.toString());
                }
            });
        });

        $(this.refs.modal.getDOMNode()).modal();
    },

    // Reset graph
    reset: function () {
        this.setFCECount(0);
        this.refs.nodes.reset();
        this.refs.bools.reset();
        this.refs.edges.reset();
    },

    renderArrowHead: function () {
        var polylineAttrs = {points: '0,1 10,5 0,9', fill: 'black'};
        return (
            <defs>
                <marker id='arrowHead' ref='marker'
                        viewBox='0 0 10 10'>
                    <polyline {... polylineAttrs}/>
                </marker>
            </defs>
        );
    },

    incrementZoom: function(increase, zoomFactorRate) {
        // onButtonRelease calls are required when a button becomes disabled
        // because it loses its ability to detect mouseUp event
        if (increase) {
            if (this.state.zoomFactor > 0.5) { // zooming allowed
                this.setState({zoomFactor: this.state.zoomFactor - zoomFactorRate});
            } else { // button becomes disabled
                this.onButtonRelease();
            }
        } else {
            if (this.state.zoomFactor < 1.1) {
                this.setState({zoomFactor: this.state.zoomFactor + zoomFactorRate});
            } else {
                this.onButtonRelease();
            }
        }
    },

    calculateRatioGraphSizeToContainerSize: function() {
        var containerWidth = document.getElementById('react-graph').clientWidth;
        var containerHeight = document.getElementById('react-graph').clientHeight;
        var heightToContainerRatio = this.state.height / containerHeight;
        var widthToContainerRatio = this.state.width / containerWidth;
        return Math.max(heightToContainerRatio, widthToContainerRatio);
    },

    graphRightEdgeOffScreen: function() {
        // Calculate right edge prior to auto adjusting to fill container.
        var rightEdge = (this.state.width - this.state.horizontalPanFactor) / this.state.zoomFactor;
        // Adjust right edge position to account for auto resize.
        rightEdge /= this.calculateRatioGraphSizeToContainerSize();
        return rightEdge > document.getElementById('react-graph').clientWidth;
    },

    graphBottomEdgeOffScreen: function() {
        // Calculate bottom edge prior to auto adjusting to fill container.
        var bottomEdge = (this.state.height - this.state.verticalPanFactor) / this.state.zoomFactor;
        // Adjust bottom edge position to account for auto resize.
        bottomEdge /= this.calculateRatioGraphSizeToContainerSize();
        return bottomEdge > document.getElementById('react-graph').clientHeight;;
    },

    graphTopEdgeOffScreen: function() {
        return this.state.verticalPanFactor > 0;
    },

    graphLeftEdgeOffScreen: function() {
        return this.state.horizontalPanFactor > 0;
    },

    panDirection: function(direction, panFactorRate) {
        // onButtonRelease calls are required when a button becomes disabled
        // because it loses its ability to detect mouseUp event
        if (direction === 'up') {
            if (this.graphTopEdgeOffScreen()) { //panning allowed
                this.setState({verticalPanFactor: this.state.verticalPanFactor - panFactorRate});
            } else { // button becomes disabled
                this.onButtonRelease();
            }
        } else if (direction === 'left') {
            if (this.graphLeftEdgeOffScreen()) {
                this.setState({horizontalPanFactor: this.state.horizontalPanFactor - panFactorRate});
            } else {
                this.onButtonRelease();
            }
        } else if (direction ==='down') {
            if (this.graphBottomEdgeOffScreen()) {
                this.setState({verticalPanFactor: this.state.verticalPanFactor + panFactorRate});
            } else {
                this.onButtonRelease();
            }
        } else if (direction === 'right') {
            if (this.graphRightEdgeOffScreen()) {
                this.setState({horizontalPanFactor: this.state.horizontalPanFactor + panFactorRate});
            } else {
                this.onButtonRelease();
            }
        }
    },

    resetZoomAndPan: function() {
        this.setState({
            zoomFactor: 1,
            verticalPanFactor: 0,
            horizontalPanFactor: 0
        });
    },

    onButtonPress: function(zoomOrPanFunction, direction, rateOfChange) {
        zoomOrPanFunction(direction, rateOfChange);
        var mouseIsDown = setInterval(() => zoomOrPanFunction(direction, rateOfChange), 500);
        this.setState({mouseDown: mouseIsDown});
    },

    onButtonRelease: function() {
        var mouseIsDown = clearInterval(this.state.mouseDown)
        this.setState({mouseDown: mouseIsDown});
    },

    onKeyDown: function(event) {
        if (event.keyCode == 39) {
            this.panDirection('right', 5);
        } else if (event.keyCode == 40) {
            this.panDirection('down', 5);
        } else if (event.keyCode == 37) {
            this.panDirection('left', 5);
        } else if (event.keyCode == 38) {
            this.panDirection('up', 5);
        }
    },

    onWheel: function(event) {
        if (event.deltaY < 0) {
            this.incrementZoom(true, 0.005);
        } else if (event.deltaY > 0) {
            this.incrementZoom(false, 0.005);
        }
    },

    render: function () {
        // not all of these properties are supported in React
        var svgAttrs = {
            width: '100%',
            height: '100%',
            viewBox: this.state.horizontalPanFactor + ' ' +
                     this.state.verticalPanFactor + ' ' +
                     (this.state.width * this.state.zoomFactor) + ' ' +
                     (this.state.height * this.state.zoomFactor),
            preserveAspectRatio: 'xMinYMin'
        };

        var zoomInDisabled = this.state.zoomFactor <= 0.5;
        var zoomOutDisabled = this.state.zoomFactor >= 1.1;
        var panUpDisabled = !this.graphTopEdgeOffScreen() ? true: false;
        var panRightDisabled = !this.graphRightEdgeOffScreen() ? true: false;
        var panDownDisabled = !this.graphBottomEdgeOffScreen() ? true: false;
        var panLeftDisabled = !this.graphLeftEdgeOffScreen() ? true: false;
        var resetDisabled = this.state.zoomFactor == 1 &&
                            this.state.horizontalPanFactor == 0 &&
                            this.state.verticalPanFactor == 0;
        return (
            <div>
                <Button
                    divId='zoom-in-button'
                    text='+'
                    mouseDown={() => this.onButtonPress(this.incrementZoom, true, 0.05)}
                    mouseUp={this.onButtonRelease}
                    disabled={zoomInDisabled}/>
                <Button
                    divId='zoom-out-button'
                    text= '&mdash;'
                    mouseDown={() => this.onButtonPress(this.incrementZoom, false, 0.05)}
                    mouseUp={this.onButtonRelease}
                    disabled={zoomOutDisabled}/>
                <Button
                    divId='pan-up-button'
                    text='↑'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'up', 10)}
                    mouseUp={this.onButtonRelease}
                    disabled={panUpDisabled}/>
                <Button
                    divId='pan-down-button'
                    text='↓'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'down', 10)}
                    mouseUp={this.onButtonRelease}
                    disabled={panDownDisabled}/>
                <Button
                    divId='pan-right-button'
                    text='→'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'right', 10)}
                    mouseUp={this.onButtonRelease}
                    disabled={panRightDisabled}/>
                <Button
                    divId='pan-left-button'
                    text='←'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'left', 10)}
                    mouseUp={this.onButtonRelease}
                    disabled={panLeftDisabled}/>
                <Button
                    divId='reset-button'
                    text='Reset'
                    mouseDown={this.resetZoomAndPan}
                    mouseUp={this.onButtonRelease}
                    disabled={resetDisabled}/>
                <Modal ref='modal' />
                <svg {... svgAttrs} ref='svg' version='1.1'
                    className={this.state.highlightedNodes.length > 0 ?
                                'highlight-nodes' : ''}>
                    {this.renderArrowHead()}
                    <RegionGroup
                        regionsJSON={this.state.regionsJSON}
                        labelsJSON={this.state.labelsJSON}/>
                    <NodeGroup
                        ref='nodes'
                        nodeClick={this.nodeClick}
                        nodeMouseEnter={this.nodeMouseEnter}
                        nodeMouseLeave={this.nodeMouseLeave}
                        svg={this}
                        nodesJSON={this.state.nodesJSON}
                        hybridsJSON={this.state.hybridsJSON}
                        edgesJSON={this.state.edgesJSON}
                        highlightedNodes={this.state.highlightedNodes}/>
                    <BoolGroup
                        ref='bools'
                        boolsJSON={this.state.boolsJSON}
                        edgesJSON={this.state.edgesJSON}
                        svg={this}/>
                    <EdgeGroup svg={this} ref='edges' edgesJSON={this.state.edgesJSON}/>
                    <InfoBox
                        ref='infoBox'
                        onClick={this.infoBoxMouseClick}
                        onMouseEnter={this.infoBoxMouseEnter}
                        onMouseLeave={this.infoBoxMouseLeave}/>
                </svg>
            </div>

        );
    }
});


// This now uses the new syntax for a stateless React component
// (component with only a render method).
// It also uses ES2015 "fat arrow" syntax for function definition.
var RegionGroup = ({regionsJSON, labelsJSON}) => (
    <g id='regions'>
        {regionsJSON.map(function (entry, value) {
            var pathAttrs = {d: 'M'};
            entry.points.forEach(function (x) {
                pathAttrs['d'] += x[0] + ',' + x[1] + ' ';
            });

            var pathStyle = {fill : entry.fill}
            return (
                <path {... pathAttrs} key={value} className='region' style={pathStyle}>
                </path>
            );
        })}
        {labelsJSON.map(function (entry, value) {
            var textAttrs = {
                x: entry.pos[0],
                y: entry.pos[1]
            };

            var textStyle = {fill : entry.fill}

            return (
                <text {... textAttrs}
                      key={value}
                      style={textStyle}
                      className='region-label'
                      textAnchor={entry['text-anchor']}>
                    {entry['text']}
               </text>
            );
        })}
    </g>
);


var NodeGroup = React.createClass({
    reset: function () {
        this.props.nodesJSON.forEach(nodeJSON => {
            var node = this.refs[nodeJSON.id_];
            var state = node.props.parents.length === 0 ? 'takeable' : 'inactive';
            node.setState({status: state, selected: false});
            setCookie(node.props.JSON.id_, state);
        });

        this.props.hybridsJSON.forEach(hybridJSON => {
            var hybrid = this.refs[hybridJSON.id_];
            var state = hybrid.props.parents.length === 0 ? 'takeable' : 'inactive';
            hybrid.setState({status: state, selected: false});
            setCookie(hybrid.props.JSON.id_, state);
        });
    },

    // Helper for hybrid computation
    findRelationship: function (course) {
        var nodes = this.props.nodesJSON;
        var node = nodes.find(n =>
            n.type_ === 'Node' &&
            n.text.some(textTag => textTag.text.includes(course)));
        return node;
    },

    render: function () {
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

    arePrereqsSatisfied: function () {
        var svg = this.props.svg;
        function isAllTrue(element) {
            if (typeof element === 'string') {
                return (svg.refs['nodes'].refs[element] ?
                        svg.refs['nodes'].refs[element].isSelected() :
                        svg.refs['bools'].refs[element].isSelected());
            } else {
                return element.some(isAllTrue);
            }
        }

        return this.props.parents.every(isAllTrue);
    },

    updateNode: function (recursive) {
        var newState;
        if (this.arePrereqsSatisfied()) {
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
            var svg = this.props.svg;
            this.setState({status: newState}, function () {
                setCookie(nodeId, newState);
                this.props.childs.forEach(function (node) {
                    var currentNode = svg.refs['nodes'].refs[node] ||
                                      svg.refs['bools'].refs[node];
                    currentNode.updateNode();
                });
                var allEdges = this.props.outEdges.concat(this.props.inEdges);
                allEdges.forEach(function (edge) {
                    var currentEdge = svg.refs['edges'].refs[edge];
                    currentEdge.updateStatus();
                }.bind(this));
            });
        } else {
            this.setState({status: newState});
            setCookie(nodeId, newState);
        }
    },

    toggleSelection: function () {
        this.setState({selected: !this.state.selected}, function () {
            this.updateNode();
        })
    },

    focusPrereqs: function () {
        var svg = this.props.svg;
        var id = this.props.JSON.id_;
        // Check if there are any missing prerequisites.
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
                    if (typeof node === 'string') {
                        var currentNode = svg.refs['nodes'].refs[node] ||
                                          svg.refs['bools'].refs[node];
                        currentNode.focusPrereqs();
                    } else {
                        node.forEach(n => {
                            var currentNode = svg.refs['nodes'].refs[n] ||
                                              svg.refs['bools'].refs[n];
                            currentNode.focusPrereqs();
                        });
                    }
                });
            });
        }
    },

    unfocusPrereqs: function () {
        var svg = this.props.svg;
        this.updateNode(false);
        this.props.parents.forEach(function (node) {
            if (typeof node === 'string') {
                var currentNode = svg.refs['nodes'].refs[node] ||
                                  svg.refs['bools'].refs[node];
                currentNode.unfocusPrereqs();
            } else {
                node.forEach(n => {
                    var currentNode = svg.refs['nodes'].refs[n] ||
                                      svg.refs['bools'].refs[n];
                    currentNode.unfocusPrereqs();
                });
            }
        });
        this.props.inEdges.forEach(function (edge) {
            var currentEdge = svg.refs['edges'].refs[edge];
            if (currentEdge.state.status === 'missing') {
                currentEdge.updateStatus();
            }
        });
    },

    render: function () {
        var newClassName = this.props.className + ' ' + this.state.status;
        if (this.props.highlighted) {
            var attrs = this.props.JSON;
            var width = parseFloat(attrs.width) / 2;
            var height = parseFloat(attrs.height) / 2;
            var ellipse = (
                <ellipse
                    className='spotlight'
                    cx={parseFloat(attrs.pos[0]) + width}
                    cy={parseFloat(attrs.pos[1]) + height}
                    rx={width + 9}
                    ry={height + 8.5} />
                );
        } else {
            var ellipse = null;
        }

        var gAttrs = {
            'text-rendering': 'geometricPrecision',
            'shape-rendering': 'geometricPrecision'
        };

        var rectAttrs = {
            height: this.props.JSON.height,
            width: this.props.JSON.width,
            rx: '4',
            ry: '4',
            x: this.props.JSON.pos[0],
            y: this.props.JSON.pos[1]
        };

        var rectStyle = {
            fill : this.props.JSON.fill
        }

        var textXOffset = this.props.JSON.pos[0] + this.props.JSON.width / 2;

        return (
            <g {... this.props} {... gAttrs}
               id={this.props.JSON.id_}
               className={newClassName} >
                {ellipse}
                <rect {... rectAttrs} style={rectStyle}/>
                {this.props.JSON.text.map(function (textTag, i) {
                    var textAttrs = {
                        x: textXOffset,
                        y: textTag.pos[1]
                    };
                    return (
                        <text {... textAttrs} key={i}>
                            {textTag.text}
                        </text>);
                })}
            </g>
        );
    }
});


var BoolGroup = React.createClass({
    componentDidMount: function () {
        for (var ref in this.refs) {
            this.refs[ref].updateNode(this.props.svg);
        }
    },

    reset: function () {
        this.props.boolsJSON.forEach((boolJSON) => {
            var bool = this.refs[boolJSON.id_];
            bool.setState({status: 'inactive'});
        });
    },

    // Generate data for a Bool node
    generateBool: function (boolJSON) {
        var parents = [];
        var childs = [];
        var outEdges = [];
        var inEdges = [];
        this.props.edgesJSON.map(function (edge) {
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
                logicalType={boolJSON.text[0].text}
                svg={this.props.svg}/>
    },

    render: function () {
        return (
            <g id='bools'>
                {this.props.boolsJSON.map(this.generateBool)}
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

    arePrereqsSatisfied: function () {
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
    },

    updateNode: function () {
        var svg = this.props.svg;
        var newState = this.arePrereqsSatisfied() ? 'active' : 'inactive';

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
                currentEdge.updateStatus();
            });
        });
    },

    focusPrereqs: function () {
        var svg = this.props.svg;
        // Check if there are any missing prerequisites.
        if (this.state.status !== 'active') {
            this.setState({status: 'missing'}, () => {
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
                    currentNode.focusPrereqs();
                });
            });
        }
    },

    unfocusPrereqs: function () {
        var svg = this.props.svg;
        this.updateNode(svg);
        this.props.parents.forEach(function (node, i) {
            var currentNode = svg.refs['nodes'].refs[node] ||
                              svg.refs['bools'].refs[node];
            currentNode.unfocusPrereqs(svg);
        });
    },

    render: function () {
        var ellipseAttrs = {
            cx: this.props.JSON.pos[0],
            cy: this.props.JSON.pos[1],
            rx: '9.8800001',
            ry: '7.3684001'
        };
        return (
            <g {... this.props.JSON}
               className={this.props.className + ' ' + this.state.status} >
                <ellipse {... ellipseAttrs}/>
                {this.props.JSON.text.map(function (textTag, i) {
                    var textAttrs = {
                        x: ellipseAttrs.cx,
                        y: textTag.pos[1]
                    };
                    return (
                        <text {... textAttrs} key={i}>
                            {this.props.logicalType}
                        </text>);
                }.bind(this))}
            </g>
        );
    }
});


var EdgeGroup = React.createClass({
    // EdgeGroup's state is used to keep track of the edgeIDs of
    // edges that are missing. Void is just a placeholder state so
    // we can declare an initial state; it does nothing.
    getInitialState: function() {
        return {};
    },

    // When an edge's state changes and the edge is not undefined,
    // it will call updateEdgeStatus and update EdgeGroup's state with its
    // edgeID and status. This function is passed as a props to Edge.
    updateEdgeStatus: function(edgeID, state) {
        var isMissing = state === 'missing';
        this.setState({[edgeID]: isMissing});
    },

    componentDidUpdate: function () {
        for (var ref in this.refs) {
            this.refs[ref].updateStatus();
        }
    },

    reset: function () {
        this.props.edgesJSON.forEach((edgeJSON) => {
            this.refs[edgeJSON.id_].setState({status: 'inactive'});
        });
    },

    // Generate data for an Edge component
    generateEdge: function (edgeJSON) {
        return <Edge className='path'
                     key={edgeJSON.id_}
                     ref={edgeJSON.id_}
                     source={edgeJSON.source}
                     target={edgeJSON.target}
                     points={edgeJSON.points}
                     svg={this.props.svg}
                     edgeID={edgeJSON.id_}
                     updateEdgeStatus={this.updateEdgeStatus} />;
    },

    render: function () {
        // Missing edges must be rendered last. The sort
        // method custom sorts a copy of edgesJSON so that all missing edges
        // are last in the list. Then render based on that list.
        var edges = this.props.edgesJSON;
        var edgesCopy = $.extend([], edges);
        var state = this.state;
        edgesCopy.sort(function(a, b) {
            // If an edge is missing, its edgeID should be in EdgeGroup's
            // state and its value should be true.
            var aID = a.id_;
            var bID = b.id_;
            var aMiss = false;
            var bMiss = false;
            aMiss = aID in state && state[aID];
            bMiss = bID in state && state[bID];
            if ((aMiss && bMiss) || (!aMiss && !bMiss)) {
                // a and b are equal
                return 0;
            } else if (aMiss && !bMiss) {
                // sort a after b
                return 1;
            } else if (!aMiss && bMiss) {
                // sort b after a
                return -1;
            }
        });
        return (
            <g id='edges'>
                {edgesCopy.map(this.generateEdge)}
            </g>
        );
    }
});


var Edge = React.createClass({
    getInitialState: function () {
        return {status: 'inactive'};
    },

    updateStatus: function () {
        var source = this.props.svg.refs.nodes.refs[this.props.source] ||
                     this.props.svg.refs.bools.refs[this.props.source];
        var target = this.props.svg.refs.nodes.refs[this.props.target] ||
                     this.props.svg.refs.bools.refs[this.props.target];
        if (!source.isSelected() && target.state.status === 'missing') {
            this.setState({status: 'missing'});
        } else if (!source.isSelected()) {
            this.setState({status: 'inactive'});
        } else if (!target.isSelected()) {
            this.setState({status: 'takeable'});
        } else {
            this.setState({status: 'active'});
        }
    },

    componentDidUpdate : function(prevProps, prevState) {
        // After each render, check if the edge's state has changed. If so,
        // notify the state of EdgeGroup with updateEdgeStatus.
        if (this.state.status !== prevState.status) {
            this.props.updateEdgeStatus(this.props.edgeID, this.state.status);
        }
    },

    render: function () {
        var pathAttrs = {d: 'M'};
        this.props.points.forEach(function (p) {
            pathAttrs.d += p[0] + ',' + p[1] + ' ';
        });

        return (
            <path {... pathAttrs}
                  className={this.props.className + ' ' + this.state.status}
                  markerEnd='url(#arrowHead)'>
            </path>
        );
    }
});


var InfoBox = React.createClass({
    getInitialState: function () {
        return {
            xPos: '0',
            yPos: '0',
            nodeId: '',
            showInfobox: false
        };
    },

    render: function () {
        if (this.state.showInfobox) {
            //TODO: move to CSS
            var gStyles = {
                cursor: 'pointer',
                transition: 'opacity .4s',
                opacity: this.state.showInfobox ? 1 : 0
            }
            var rectAttrs = {
                id:this.state.nodeId+'-tooltip' + '-rect',
                x: this.state.xPos,
                y: this.state.yPos,
                rx: '4',
                ry: '4',
                fill: 'white',
                stroke: 'black',
                'stroke-width': '2',
                width: '60',
                height: '30'
            };

            var textAttrs = {
                'id': this.state.nodeId +'-tooltip' + '-text',
                'x': parseFloat(this.state.xPos) + 60 / 2 - 18,
                'y': parseFloat(this.state.yPos) + 30 / 2 + 6
            };

            return (
                <g id='infoBox' className='tooltip-group' style={gStyles} {... this.props}>
                    <rect {... rectAttrs} ></rect>
                    <text {... textAttrs} >
                        Info
                    </text>
                </g>
            );
        } else {
            return <g></g>;
        }
    }
});

export default {renderReactGraph: renderReactGraph};
