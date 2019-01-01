import React from 'react';
import ReactDOM from 'react-dom';
import { Modal } from '../common/react_modal.js.jsx';
import { ExportModal } from '../common/export.js.jsx';


/**
 * Search for target node in list of nodes,
 * or if node not found search through list of bools.
 * @param {React.PropTypes.node} targetNode
 * @param {React.PropTypes.element} elem
 * @returns {React.PropTypes.Node}
 */
function refLookUp(targetNode, svg) {
    return svg.refs['nodes'].refs[targetNode] ||
        svg.refs['bools'].refs[targetNode];
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


function Button(props) {
    return (
        <button id={props.divId} className='graph-control-button'
        onMouseDown={props.mouseDown}
        onMouseUp={props.mouseUp}
        onMouseEnter={props.onMouseEnter}
        onMouseLeave={props.onMouseLeave}
        disabled={props.disabled}>{props.text}</button>
    );
}


export function renderReactGraph(graph_container_id, start_blank, edit) {
    if (start_blank === undefined) {
        start_blank = false;
    }

    // If edit is NOT undefined, then the user is on the draw page
    if (edit === undefined) {
        edit = false;
    }

    return ReactDOM.render(
        <Graph
            start_blank={start_blank}
            edit={edit}
            initialOnDraw={edit}
            initialDrawMode='draw-node' />,
        document.getElementById(graph_container_id)
    );
}


class Graph extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            labelsJSON: [],
            regionsJSON: [],
            nodesJSON: [],
            hybridsJSON: [],
            boolsJSON: [],
            edgesJSON: [],
            highlightedNodes: [],
            timeouts: [],
            fceCount: 0,
            width: window.innerWidth,
            height: window.innerHeight,
            zoomFactor: 1,
            horizontalPanFactor: 0,
            verticalPanFactor: 0,
            mouseDown: false,
            buttonHover: false,
            onDraw: this.props.initialOnDraw,
            drawMode: this.props.initialDrawMode,
            drawNodeID: 0,
            draggingNode: null
        };
    }

    componentDidMount() {
        if (!this.props.start_blank) {
            this.getGraph();
        }

        // can't detect keydown event when adding event listener to react-graph
        document.body.addEventListener('keydown', this.onKeyDown);
        document.getElementById('react-graph').addEventListener('wheel', this.onWheel);

        // Enable "Export" link
        $('#nav-export').click(() => this.exportModal.openModal());

        // Need to hardcode these in because React does not understand these attributes
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
    }

    componentWillUnmount() {
        document.body.removeEventListener('keydown', this.onKeyDown);
        document.getElementById('react-graph').removeEventListener('wheel', this.onWheel);
    }

    getGraph = (graphName) => {
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
                graphName = localStorage.getItem('active-graph') || 'Computer Science';
            }
        }

        graphName = graphName.replace('-', ' ');

        $.ajax({
            dataType: 'json',
            url: 'get-json-data',
            data: {'graphName': graphName},
            success: function (data) {
                localStorage.setItem('active-graph', graphName);
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
    }

    componentDidUpdate(prevProps, prevState) {
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
    }

    clearAllTimeouts = () => {
        for (var i = 0; i < this.state.timeouts.length; i++) {
            clearTimeout(this.state.timeouts[i]);
        }

        this.setState({timeouts: []});
    }

    setFCECount = (credits) => {
        this.setState({fceCount: credits}, function () {
            $('#fcecount').text('FCE Count: ' + this.state.fceCount);
        });
    }

    incrementFCECount = (credits) => {
        this.setState({fceCount: this.state.fceCount + credits}, function () {
            $('#fcecount').text('FCE Count: ' + this.state.fceCount);
        });
    }

    nodeClick = (event) => {
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
    }

    nodeMouseEnter = (event) => {
        var courseId = event.currentTarget.id;
        var currentNode = this.refs.nodes.refs[courseId];
        currentNode.focusPrereqs(this);

        this.clearAllTimeouts();

        var infoBox = this.infoBox;

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

        if (!this.state.onDraw) {
            infoBox.setState({xPos: xPos,
                              yPos: yPos,
                              nodeId: courseId,
                              showInfobox: true});
        }
        this.setState({buttonHover: true});
    }

    nodeMouseLeave = (event) => {
        var courseId = event.currentTarget.id;
        var currentNode = this.refs.nodes.refs[courseId];
        currentNode.unfocusPrereqs(this);

        var infoBox = this.infoBox;

        var timeout = setTimeout(function () {
                infoBox.setState({showInfobox: false});
        }, 400);


        this.setState({timeouts: this.state.timeouts.concat(timeout),
            buttonHover: false});
    }

    nodeMouseDown = (event) => {
        if (this.state.drawMode === 'draw-node' && event.currentTarget.id.startsWith('n')) {
            this.setState({draggingNode: event.currentTarget.id});
        }
    }

    drawMouseMove = (event) => {
        // in draw-node mode, drag a node as the mouse moves
        if (this.state.drawMode === 'draw-node') {
            if (this.state.draggingNode !== null) {
                var newPos = this.getRelativeCoords(event);
                var currentNode;
                for (var node of this.state.nodesJSON) {
                    if (node.id_ === this.state.draggingNode) {
                        currentNode = node;
                    }
                }
                currentNode.pos = [newPos.x-20, newPos.y-15];
                currentNode.text[0].pos = [newPos.x, newPos.y+5];
                var newNodesJSON = $.extend([], this.state.nodesJSON);
                newNodesJSON.push(currentNode);
                this.setState({nodesJSON: newNodesJSON});
            }
        }
    };

    drawMouseUp = (event) => {
        // in draw-node mode, drop a dragged node to a new location
        if (this.state.drawMode === 'draw-node') {
            if (this.state.draggingNode !== null) {
                var newPos = this.getRelativeCoords(event);
                var currentNode;
                for (var node of this.state.nodesJSON) {
                    if (node.id_ === this.state.draggingNode) {
                        currentNode = node;
                    }
                }
                currentNode.pos = [newPos.x-20, newPos.y-15];
                currentNode.text[0].pos = [newPos.x, newPos.y+5];
                var newNodesJSON = $.extend([], this.state.nodesJSON);
                newNodesJSON.push(currentNode);
                this.setState({nodesJSON: newNodesJSON,
                    draggingNode: null});
            }
        }
    }

    infoBoxMouseEnter = () => {
        this.clearAllTimeouts();

        var infoBox = this.infoBox;
        infoBox.setState({showInfobox: true});
    }

    infoBoxMouseLeave = () => {
        var infoBox = this.infoBox;

        var timeout = setTimeout(function () {
                infoBox.setState({showInfobox: false});
        }, 400);

        this.setState({timeouts: this.state.timeouts.concat(timeout)});
    }

    infoBoxMouseClick = () => {
        var infoBox = this.infoBox;
        var newCourse = infoBox.state.nodeId.substring(0, 6);
        console.log(newCourse);
        this.setState({courseId: newCourse});;
        this.modal.openModal(newCourse);
    }

    openExportModal = () => {
        this.exportModal.openModal();
    }

    // Reset graph
    reset = () => {
        this.setFCECount(0);
        this.refs.nodes.reset();
        this.refs.bools.reset();
        this.refs.edges.reset();
    }

    renderArrowHead = () => {
        var polylineAttrs = {points: '0,1 10,5 0,9', fill: 'black'};
        return (
            <defs>
                <marker id='arrowHead' ref='marker'
                        viewBox='0 0 10 10'>
                    <polyline {... polylineAttrs}/>
                </marker>
            </defs>
        );
    }

    incrementZoom = (increase, zoomFactorRate) => {
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
    }

    calculateRatioGraphSizeToContainerSize = () => {
        var containerWidth = document.getElementById('react-graph').clientWidth;
        var containerHeight = document.getElementById('react-graph').clientHeight;
        var heightToContainerRatio = this.state.height / containerHeight;
        var widthToContainerRatio = this.state.width / containerWidth;
        return Math.max(heightToContainerRatio, widthToContainerRatio);
    }

    graphRightEdgeOffScreen = () => {
        // Calculate right edge prior to auto adjusting to fill container.
        var rightEdge = (this.state.width - this.state.horizontalPanFactor) / this.state.zoomFactor;
        // Adjust right edge position to account for auto resize.
        rightEdge /= this.calculateRatioGraphSizeToContainerSize();
        return rightEdge > document.getElementById('react-graph').clientWidth;
    }

    graphBottomEdgeOffScreen = () => {
        // Calculate bottom edge prior to auto adjusting to fill container.
        var bottomEdge = (this.state.height - this.state.verticalPanFactor) / this.state.zoomFactor;
        // Adjust bottom edge position to account for auto resize.
        bottomEdge /= this.calculateRatioGraphSizeToContainerSize();
        return bottomEdge > document.getElementById('react-graph').clientHeight;;
    }

    graphTopEdgeOffScreen = () => {
        return this.state.verticalPanFactor > 0;
    }

    graphLeftEdgeOffScreen = () => {
        return this.state.horizontalPanFactor > 0;
    }

    panDirection = (direction, panFactorRate) => {
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
    }

    resetZoomAndPan = () => {
        this.setState({
            zoomFactor: 1,
            verticalPanFactor: 0,
            horizontalPanFactor: 0
        });
    }

    onButtonPress = (zoomOrPanFunction, direction, rateOfChange) => {
        zoomOrPanFunction(direction, rateOfChange);
        var mouseIsDown = setInterval(() => zoomOrPanFunction(direction, rateOfChange), 500);
        this.setState({mouseDown: mouseIsDown});
    }

    onButtonRelease = () => {
        var mouseIsDown = clearInterval(this.state.mouseDown)
        this.setState({mouseDown: mouseIsDown});
    }

    onKeyDown = (event) => {
        if (event.keyCode == 39) {
            this.panDirection('right', 5);
        } else if (event.keyCode == 40) {
            this.panDirection('down', 5);
        } else if (event.keyCode == 37) {
            this.panDirection('left', 5);
        } else if (event.keyCode == 38) {
            this.panDirection('up', 5);
        } else if (this.state.onDraw) {
            if (event.keyCode == 78) {
                this.setState({drawMode: 'draw-node'});
            }
        }
    }

    onWheel = (event) => {
        if (event.deltaY < 0) {
            this.incrementZoom(true, 0.005);
        } else if (event.deltaY > 0) {
            this.incrementZoom(false, 0.005);
        }
    }

    buttonMouseEnter = () => {
        this.setState({buttonHover: true});
    }

    buttonMouseLeave = () => {
        this.setState({buttonHover: false});
    }

    getRelativeCoords = (event) => {
        var x = event.nativeEvent.offsetX;
        var y = event.nativeEvent.offsetY;
        x = (x * this.state.zoomFactor) + this.state.horizontalPanFactor;
        y = (y * this.state.zoomFactor) + this.state.verticalPanFactor;
        return {x: x, y: y};
    }

    drawNode = (x, y) => {
        var xPos, yPos;

        // if node would extend offscreen, instead place it at the
        // edge. Give 2 pixels extra for node border width.
        if (x+42 > this.state.width) {
            xPos = this.state.width-42;
        } else if (x < 2) {
            xPos = 2;
        } else {
            xPos = x;
        }

        if (y+34 > this.state.height) {
            yPos = this.state.height-34;
        } else if (y < 2) {
            yPos = 2;
        } else {
            yPos = y;
        }

        // text is an empty string for now until implementation,
        // text position uses node position for now
        var textJSON = {
            'align': 'begin',
            'fill': '',
            'graph': 0,
            'pos': [xPos, yPos+20],
            'rId': 'text' + this.state.drawNodeID,
            'text': 'la'
        }

        var nodeJSON = {
            'fill': '#' + $('#select-colour').val(),
            'graph': 0,
            // default dimensions for a node
            'height': 32,
            'width': 40,
            'id_': 'n' + this.state.drawNodeID,
            'pos': [xPos, yPos],
            'stroke': '',
            'text': [textJSON],
            'tolerance': 9,
            'type_': 'Node'
        };

        var newNodesJSON = $.extend([], this.state.nodesJSON);
        newNodesJSON.push(nodeJSON);
        this.setState({nodesJSON: newNodesJSON,
            drawNodeID: this.state.drawNodeID + 1});
    }

    /**
    * In draw-node creates a new node at the position of the click event on the SVG canvas.
    * In path-mode creates an elbow at the position of the click event on the SVG canvas,
      if the startNode is defined.
    * @param {object} e The mousedown event.
    */
    drawGraphObject = (e) => {
        var pos = this.getRelativeCoords(e);
        // check if the user is trying to draw a node. Also check
        // if the user is trying to press a button instead (ie zoom buttons)
        if (this.state.drawMode === 'draw-node' &&
            !this.state.buttonHover) {
            this.drawNode(pos.x, pos.y);
        }
    }

    render() {
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


        // Mouse events for draw tool
        var mouseEvents = {}
        if (this.state.onDraw) {
            mouseEvents = {
                'onMouseDown': this.drawGraphObject,
                'onMouseUp': this.drawMouseUp,
                'onMouseMove': this.drawMouseMove
            };
        }

        return (
            <div>
                <Modal ref={r => this.modal = r} />
                <ExportModal
                    context='graph'
                    session=''
                    ref={r => this.exportModal = r} />
                <Button
                    divId='zoom-in-button'
                    text='+'
                    mouseDown={() => this.onButtonPress(this.incrementZoom, true, 0.05)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={zoomInDisabled}/>
                <Button
                    divId='zoom-out-button'
                    text= '&mdash;'
                    mouseDown={() => this.onButtonPress(this.incrementZoom, false, 0.05)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={zoomOutDisabled}/>
                <Button
                    divId='pan-up-button'
                    text='↑'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'up', 10)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={panUpDisabled}/>
                <Button
                    divId='pan-down-button'
                    text='↓'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'down', 10)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={panDownDisabled}/>
                <Button
                    divId='pan-right-button'
                    text='→'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'right', 10)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={panRightDisabled}/>
                <Button
                    divId='pan-left-button'
                    text='←'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'left', 10)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={panLeftDisabled}/>
                <Button
                    divId='reset-button'
                    text='Reset'
                    mouseDown={this.resetZoomAndPan}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={resetDisabled}/>

                <svg {... svgAttrs} ref='svg' version='1.1'
                    className={this.state.highlightedNodes.length > 0 ?
                                'highlight-nodes' : ''}
                    {... mouseEvents } >
                    {this.renderArrowHead()}
                    <RegionGroup
                        regionsJSON={this.state.regionsJSON}
                        labelsJSON={this.state.labelsJSON}/>
                    <NodeGroup
                        ref='nodes'
                        nodeClick={this.nodeClick}
                        nodeMouseEnter={this.nodeMouseEnter}
                        nodeMouseLeave={this.nodeMouseLeave}
                        nodeMouseDown={this.nodeMouseDown}
                        svg={this}
                        nodesJSON={this.state.nodesJSON}
                        hybridsJSON={this.state.hybridsJSON}
                        edgesJSON={this.state.edgesJSON}
                        highlightedNodes={this.state.highlightedNodes}
                        onDraw={this.state.onDraw}/>
                    <BoolGroup
                        ref='bools'
                        boolsJSON={this.state.boolsJSON}
                        edgesJSON={this.state.edgesJSON}
                        svg={this}/>
                    <EdgeGroup svg={this} ref='edges' edgesJSON={this.state.edgesJSON}/>
                    <InfoBox
                        ref={r => this.infoBox = r}
                        onClick={this.infoBoxMouseClick}
                        onMouseEnter={this.infoBoxMouseEnter}
                        onMouseLeave={this.infoBoxMouseLeave}/>
                </svg>
            </div>

        );
    }
}


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


class NodeGroup extends React.Component {
    constructor(props) {
        super(props);
        this.reset = this.reset.bind(this);
        this.findRelationship = this.findRelationship.bind(this);
    }

    reset() {
        this.props.nodesJSON.forEach(nodeJSON => {
            var node = this.refs[nodeJSON.id_];
            var state = node.props.parents.length === 0 ? 'takeable' : 'inactive';
            node.setState({status: state, selected: false});
            localStorage.setItem(node.props.JSON.id_, state);
        });

        this.props.hybridsJSON.forEach(hybridJSON => {
            var hybrid = this.refs[hybridJSON.id_];
            var state = hybrid.props.parents.length === 0 ? 'takeable' : 'inactive';
            hybrid.setState({status: state, selected: false});
            localStorage.setItem(hybrid.props.JSON.id_, state);
        });
    }

    // Helper for hybrid computation
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


class Node extends React.Component {
    constructor(props) {
        super(props);
        var state = localStorage.getItem(this.props.JSON.id_);
        if (!this.props.onDraw) {
            state = '';
        } else if (state === null) {
            state = this.props.parents.length === 0 ? 'takeable' : 'inactive';
        }
        this.state = {
            status: state,
            selected: ['active', 'overridden'].indexOf(state) >= 0
        };
        this.isSelected = this.isSelected.bind(this);
        this.arePrereqsSatisfied = this.arePrereqsSatisfied.bind(this);
        this.updateNode = this.updateNode.bind(this);
        this.toggleSelection = this.toggleSelection.bind(this);
        this.focusPrereqs = this.focusPrereqs.bind(this);
        this.unfocusPrereqs = this.unfocusPrereqs.bind(this);
    }

    isSelected() {
        if (this.props.hybrid) {
            return this.state.status === 'active';
        } else {
            return this.state.selected;
        }
    }

    arePrereqsSatisfied() {
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
    }

    updateNode(recursive) {
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
            localStorage.setItem(nodeId, newState);
            this.setState({status: newState});
            return;
        }

        if (recursive === undefined || recursive) {
            var svg = this.props.svg;
            this.setState({status: newState}, function () {
                localStorage.setItem(nodeId, newState);
                this.props.childs.forEach(function (node) {
                    var currentNode = refLookUp(node, svg);
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
            localStorage.setItem(nodeId, newState);
        }
    }

    toggleSelection() {
        this.setState({selected: !this.state.selected}, function () {
            this.updateNode();
        })
    }

    focusPrereqs() {
        var svg = this.props.svg;
        var id = this.props.JSON.id_;
        // Check if there are any missing prerequisites.
        if (['inactive', 'overridden', 'takeable'].indexOf(this.state.status) >= 0) {
            this.setState({status: 'missing'}, () => {
                this.props.inEdges.forEach((edge) => {
                    var currentEdge = svg.refs['edges'].refs[edge];
                    var sourceNode = refLookUp(currentEdge.props.source, svg);
                    if (!sourceNode.isSelected()) {
                        currentEdge.setState({status: 'missing'});
                    }
                });
                var isHybrid = this.props.hybrid;
                this.props.parents.forEach((node) => {
                    if (typeof node === 'string') {
                        var currentNode = refLookUp(node, svg);
                        currentNode.focusPrereqs();
                    } else {
                        node.forEach(n => {
                            var currentNode = refLookUp(n, svg);
                            currentNode.focusPrereqs();
                        });
                    }
                });
            });
        }
    }

    unfocusPrereqs() {
        var svg = this.props.svg;
        this.updateNode(false);
        this.props.parents.forEach(function (node) {
            if (typeof node === 'string') {
                var currentNode = refLookUp(node, svg);
                currentNode.unfocusPrereqs();
            } else {
                node.forEach(n => {
                    var currentNode = refLookUp(n, svg);
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
    }

    render() {
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
            'textRendering': 'geometricPrecision',
            'shapeRendering': 'geometricPrecision',
            'onKeyDown': this.props.svg.onKeyDown,
            'onWheel': this.props.svg.onWheel,
            'onMouseEnter': this.props.onMouseEnter,
            'onMouseLeave': this.props.onMouseLeave,
            'onClick': this.props.onClick,
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

        // TODO: Look at this.props to see what we need to give the g
        return (
            <g {... gAttrs}

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
}


class BoolGroup extends React.Component {
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
            bool.setState({status: 'inactive'});
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
                svg={this.props.svg}/>
    }

    render() {
        return (
            <g id='bools'>
                {this.props.boolsJSON.map(this.generateBool)}
            </g>
        );
    }
}


class Bool extends React.Component {
    constructor(props) {
        super(props);
        this.state = {status: 'inactive'};
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
        this.setState({status: newState}, function () {
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
            this.setState({status: 'missing'}, () => {
                this.props.inEdges.forEach(function (edge) {
                    var currentEdge = svg.refs['edges'].refs[edge];
                    var sourceNode = refLookUp(currentEdge.props.source, svg);
                    if (!sourceNode.isSelected()) {
                        currentEdge.setState({status: 'missing'});
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
}


class EdgeGroup extends React.Component {
    constructor(props) {
        super(props);
        // EdgeGroup's state is used to keep track of the edgeIDs of
        // edges that are missing. Void is just a placeholder state so
        // we can declare an initial state; it does nothing.
        this.state = {}
        this.updateEdgeStatus = this.updateEdgeStatus.bind(this);
        this.reset = this.reset.bind(this);
        this.generateEdge = this.generateEdge.bind(this);
    }

    // When an edge's state changes and the edge is not undefined,
    // it will call updateEdgeStatus and update EdgeGroup's state with its
    // edgeID and status. This function is passed as a props to Edge.
    updateEdgeStatus(edgeID, state) {
        var isMissing = state === 'missing';
        this.setState({[edgeID]: isMissing});
    }

    componentDidUpdate() {
        for (var ref in this.refs) {
            this.refs[ref].updateStatus();
        }
    }

    reset() {
        this.props.edgesJSON.forEach((edgeJSON) => {
            this.refs[edgeJSON.id_].setState({status: 'inactive'});
        });
    }

    // Generate data for an Edge component
    generateEdge(edgeJSON) {
        return <Edge className='path'
                     key={edgeJSON.id_}
                     ref={edgeJSON.id_}
                     source={edgeJSON.source}
                     target={edgeJSON.target}
                     points={edgeJSON.points}
                     svg={this.props.svg}
                     edgeID={edgeJSON.id_}
                     updateEdgeStatus={this.updateEdgeStatus} />;
    }

    render() {
        // Missing edges must be rendered last. The sort
        // method custom sorts a copy of edgesJSON so that all missing edges
        // are last in the list. Then render based on that list.
        var edges = this.props.edgesJSON;
        var edgesCopy = $.extend([], edges);
        var state = this.state;
        edgesCopy.sort((a, b) => {
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
}


class Edge extends React.Component {
    constructor(props) {
        super(props);
        this.state = {status: 'inactive'};
        this.updateStatus = this.updateStatus.bind(this);
    }

    updateStatus() {
        var source = refLookUp(this.props.source, this.props.svg);
        var target = refLookUp(this.props.target, this.props.svg);
        if (!source.isSelected() && target.state.status === 'missing') {
            this.setState({status: 'missing'});
        } else if (!source.isSelected()) {
            this.setState({status: 'inactive'});
        } else if (!target.isSelected()) {
            this.setState({status: 'takeable'});
        } else {
            this.setState({status: 'active'});
        }
    }

    componentDidUpdate(prevProps, prevState) {
        // After each render, check if the edge's state has changed. If so,
        // notify the state of EdgeGroup with updateEdgeStatus.
        if (this.state.status !== prevState.status) {
            this.props.updateEdgeStatus(this.props.edgeID, this.state.status);
        }
    }

    render() {
        var pathAttrs = {d: 'M'};
        this.props.points.forEach((p) => {
            pathAttrs.d += p[0] + ',' + p[1] + ' ';
        });

        return (
            <path {... pathAttrs}
                  className={this.props.className + ' ' + this.state.status}
                  markerEnd='url(#arrowHead)'>
            </path>
        );
    }
}


class InfoBox extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            xPos: '0',
            yPos: '0',
            nodeId: '',
            showInfobox: false
        };
    }

    render() {
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
                'strokeWidth': '2',
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
}