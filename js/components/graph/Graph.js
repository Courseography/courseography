import React from 'react';
import ReactDOM from 'react-dom';
import { Modal } from '../common/react_modal.js.jsx';
import { ExportModal } from '../common/export.js.jsx';
import BoolGroup from './BoolGroup';
import Button from './Button';
import EdgeGroup from './EdgeGroup';
import InfoBox from './InfoBox';
import NodeGroup from './NodeGroup';
import RegionGroup from './RegionGroup';

export default class Graph extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            labelsJSON: props.labelsJSON || [],
            regionsJSON: props.regionsJSON || [],
            nodesJSON: props.nodesJSON || [],
            hybridsJSON: props.hybridsJSON || [],
            boolsJSON: props.boolsJSON || [],
            edgesJSON: props.edgesJSON || [],
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
        document.getElementById('nav-export').click(() => this.exportModal.openModal());

        // Need to hardcode these in because React does not understand these attributes
        var svgNode = ReactDOM.findDOMNode(this.refs.svg);
        var markerNode = ReactDOM.findDOMNode(this.refs.marker);

        svgNode.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
        svgNode.setAttribute('xmlns:svg', 'http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:dc', 'http://purl.org/dc/elements/1.1/');
        svgNode.setAttribute('xmlns:cc', 'http://creativecommons.org/ns#');
        svgNode.setAttribute('xmlns:rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');

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

        graphName = graphName.replace("-", " ").replace(" ", "%20");

        const res = fetch(`get-json-data?graphName=${graphName}`)
            .then((res) => {
                if (!res.ok) {
                    // can't just return res
                    return Promise.reject({
                        status: res.status,
                        statusText: res.statusText,
                        type: res.type,
                        url: res.url
                    });
                }
                return res.json();  // only received headers, waiting for data
            })
            .then((data) => {
                localStorage.setItem("active-graph", graphName);
                var regionsList = [];
                var nodesList = [];
                var hybridsList = [];
                var boolsList = [];
                var edgesList = [];

                var labelsList = data.texts.filter(function (entry) {
                    return entry.rId.startsWith("tspan");
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
            })
            .catch((err) => {
                console.log("Fetch API failed.");
                console.log("Here are the headers of the failed response:");
                console.error(err);
            })
        // Need to hardcode these in because React does not understand these
        // attributes
        var svgNode = ReactDOM.findDOMNode(this.refs.svg);
        var markerNode = ReactDOM.findDOMNode(this.refs.marker);

        svgNode.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
        svgNode.setAttribute('xmlns:svg', 'http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:dc', 'http://purl.org/dc/elements/1.1/');
        svgNode.setAttribute('xmlns:cc', 'http://creativecommons.org/ns#');
        svgNode.setAttribute('xmlns:rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');

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

        this.setState({ timeouts: [] });
    }

    setFCECount = (credits) => {
        this.setState({ fceCount: credits }, function () {
            document.getElementById('fcecount').textContent = 'FCE Count: ' + this.state.fceCount;
        });
    }

    incrementFCECount = (credits) => {
        this.setState({ fceCount: this.state.fceCount + credits }, function () {
            document.getElementById('fcecount').textContent = 'FCE Count: ' + this.state.fceCount;
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
            infoBox.setState({
                xPos: xPos,
                yPos: yPos,
                nodeId: courseId,
                showInfobox: true
            });
        }
        this.setState({ buttonHover: true });
    }

    nodeMouseLeave = (event) => {
        var courseId = event.currentTarget.id;
        var currentNode = this.refs.nodes.refs[courseId];
        currentNode.unfocusPrereqs(this);

        var infoBox = this.infoBox;

        var timeout = setTimeout(function () {
            infoBox.setState({ showInfobox: false });
        }, 400);


        this.setState({
            timeouts: this.state.timeouts.concat(timeout),
            buttonHover: false
        });
    }

    nodeMouseDown = (event) => {
        if (this.state.drawMode === 'draw-node' && event.currentTarget.id.startsWith('n')) {
            this.setState({ draggingNode: event.currentTarget.id });
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
                currentNode.pos = [newPos.x - 20, newPos.y - 15];
                currentNode.text[0].pos = [newPos.x, newPos.y + 5];
                var newNodesJSON = [ ...this.state.nodesJSON];
                newNodesJSON.push(currentNode);
                this.setState({ nodesJSON: newNodesJSON });
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
                currentNode.pos = [newPos.x - 20, newPos.y - 15];
                currentNode.text[0].pos = [newPos.x, newPos.y + 5];
                var newNodesJSON = [ ...this.state.nodesJSON];
                newNodesJSON.push(currentNode);
                this.setState({
                    nodesJSON: newNodesJSON,
                    draggingNode: null
                });
            }
        }
    }

    infoBoxMouseEnter = () => {
        this.clearAllTimeouts();

        var infoBox = this.infoBox;
        infoBox.setState({ showInfobox: true });
    }

    infoBoxMouseLeave = () => {
        var infoBox = this.infoBox;

        var timeout = setTimeout(function () {
            infoBox.setState({ showInfobox: false });
        }, 400);

        this.setState({ timeouts: this.state.timeouts.concat(timeout) });
    }

    infoBoxMouseClick = () => {
        var infoBox = this.infoBox;
        var newCourse = infoBox.state.nodeId.substring(0, 6);
        console.log(newCourse);
        this.setState({ courseId: newCourse });;
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
        var polylineAttrs = { points: '0,1 10,5 0,9', fill: 'black' };
        return (
            <defs>
                <marker id='arrowHead' ref='marker'
                    viewBox='0 0 10 10'>
                    <polyline {...polylineAttrs} />
                </marker>
            </defs>
        );
    }

    incrementZoom = (increase, zoomFactorRate) => {
        // onButtonRelease calls are required when a button becomes disabled
        // because it loses its ability to detect mouseUp event
        if (increase) {
            if (this.state.zoomFactor > 0.5) { // zooming allowed
                this.setState({ zoomFactor: this.state.zoomFactor - zoomFactorRate });
            } else { // button becomes disabled
                this.onButtonRelease();
            }
        } else {
            if (this.state.zoomFactor < 1.1) {
                this.setState({ zoomFactor: this.state.zoomFactor + zoomFactorRate });
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
                this.setState({ verticalPanFactor: this.state.verticalPanFactor - panFactorRate });
            } else { // button becomes disabled
                this.onButtonRelease();
            }
        } else if (direction === 'left') {
            if (this.graphLeftEdgeOffScreen()) {
                this.setState({ horizontalPanFactor: this.state.horizontalPanFactor - panFactorRate });
            } else {
                this.onButtonRelease();
            }
        } else if (direction === 'down') {
            if (this.graphBottomEdgeOffScreen()) {
                this.setState({ verticalPanFactor: this.state.verticalPanFactor + panFactorRate });
            } else {
                this.onButtonRelease();
            }
        } else if (direction === 'right') {
            if (this.graphRightEdgeOffScreen()) {
                this.setState({ horizontalPanFactor: this.state.horizontalPanFactor + panFactorRate });
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
        this.setState({ mouseDown: mouseIsDown });
    }

    onButtonRelease = () => {
        var mouseIsDown = clearInterval(this.state.mouseDown)
        this.setState({ mouseDown: mouseIsDown });
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
                this.setState({ drawMode: 'draw-node' });
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
        this.setState({ buttonHover: true });
    }

    buttonMouseLeave = () => {
        this.setState({ buttonHover: false });
    }

    getRelativeCoords = (event) => {
        var x = event.nativeEvent.offsetX;
        var y = event.nativeEvent.offsetY;
        x = (x * this.state.zoomFactor) + this.state.horizontalPanFactor;
        y = (y * this.state.zoomFactor) + this.state.verticalPanFactor;
        return { x: x, y: y };
    }

    drawNode = (x, y) => {
        var xPos, yPos;

        // if node would extend offscreen, instead place it at the
        // edge. Give 2 pixels extra for node border width.
        if (x + 42 > this.state.width) {
            xPos = this.state.width - 42;
        } else if (x < 2) {
            xPos = 2;
        } else {
            xPos = x;
        }

        if (y + 34 > this.state.height) {
            yPos = this.state.height - 34;
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
            'pos': [xPos, yPos + 20],
            'rId': 'text' + this.state.drawNodeID,
            'text': 'la'
        }

        var nodeJSON = {
            'fill': '#' + document.getElementById('select-colour').val(),
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

        var newNodesJSON = [ ...this.state.nodesJSON];
        newNodesJSON.push(nodeJSON);
        this.setState({
            nodesJSON: newNodesJSON,
            drawNodeID: this.state.drawNodeID + 1
        });
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
        var panUpDisabled = !this.graphTopEdgeOffScreen() ? true : false;
        var panRightDisabled = !this.graphRightEdgeOffScreen() ? true : false;
        var panDownDisabled = !this.graphBottomEdgeOffScreen() ? true : false;
        var panLeftDisabled = !this.graphLeftEdgeOffScreen() ? true : false;
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
                    disabled={zoomInDisabled} />
                <Button
                    divId='zoom-out-button'
                    text='&mdash;'
                    mouseDown={() => this.onButtonPress(this.incrementZoom, false, 0.05)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={zoomOutDisabled} />
                <Button
                    divId='pan-up-button'
                    text='↑'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'up', 10)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={panUpDisabled} />
                <Button
                    divId='pan-down-button'
                    text='↓'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'down', 10)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={panDownDisabled} />
                <Button
                    divId='pan-right-button'
                    text='→'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'right', 10)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={panRightDisabled} />
                <Button
                    divId='pan-left-button'
                    text='←'
                    mouseDown={() => this.onButtonPress(this.panDirection, 'left', 10)}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={panLeftDisabled} />
                <Button
                    divId='reset-button'
                    text='Reset'
                    mouseDown={this.resetZoomAndPan}
                    mouseUp={this.onButtonRelease}
                    onMouseEnter={this.buttonMouseEnter}
                    onMouseLeave={this.buttonMouseLeave}
                    disabled={resetDisabled} />

                <svg {...svgAttrs} ref='svg' version='1.1'
                    className={this.state.highlightedNodes.length > 0 ?
                        'highlight-nodes' : ''}
                    {...mouseEvents} >
                    {this.renderArrowHead()}
                    <RegionGroup
                        regionsJSON={this.state.regionsJSON}
                        labelsJSON={this.state.labelsJSON} />
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
                        onDraw={this.state.onDraw} />
                    <BoolGroup
                        ref='bools'
                        boolsJSON={this.state.boolsJSON}
                        edgesJSON={this.state.edgesJSON}
                        svg={this} />
                    <EdgeGroup svg={this} ref='edges' edgesJSON={this.state.edgesJSON} />
                    <InfoBox
                        ref={r => this.infoBox = r}
                        onClick={this.infoBoxMouseClick}
                        onMouseEnter={this.infoBoxMouseEnter}
                        onMouseLeave={this.infoBoxMouseLeave} />
                </svg>
            </div>

        );
    }
}

/**
 * Gets the value of a parameter in the query string by name.
 * @param name The name of the parameter to retrieve.
 * @returns {string|null} The value of the parameter as a string, or null if it does not exist.
 */
function getURLParameter(name) {
    'use strict';

    return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search) || [, ""])[1].replace(/\+/g, '%20')) || null;
}
