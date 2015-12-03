function renderReactGraph() {
    'use strict';
    React.render(
        <ReactSVG width={1195} height={650}/>,
        document.getElementById('react-graph')
    );
}

function getAttributes(svgAttributes) {
    'use strict';
    var attrs = [];
    //Traversing a NodeNamedMap type
    //Using a for loop instead of converting to Array
    for (var i = 0; i < svgAttributes.length; i++) {
        var item = svgAttributes[i];
        //Will be hard-coding in className and textAnchor and markerEnd
        if (item.name!='class' && item.name!='text-anchor' && item.name!='marker-end'){
            attrs[item.name] = item.value; 
        }
    }
    return attrs;
}

function getStyles(stylesStrings) {
    'use strict';
    var styles = {};
    //Have to check if it is null since null can't be split.
    if (!stylesStrings) {
        return styles;
    }
    stylesStrings.split(';').map(function(key, value) {
        if (key) {
            styles[key.substring(0, key.indexOf(':'))] = key.substring(key.indexOf(':') + 1);
        }
    });
    return styles;
}

function getNodes(mode){
    'use strict';
    //LATER: Add AJAX code to pull code here
    //In the long run, remove SVGGenerator
    var dictList = [];
    $(mode).map(function(key, element) {
        var entry = {};
        entry['id'] = element.id;
        entry['attributes'] = getAttributes(element.attributes);
        //<g> themselves currently have no styles, only the <rect> child has styles.
        //The line below returns an empty Object, this is in case we were to add styles later on
        entry['style'] = getStyles(entry['attributes']['style']);
        entry['children'] = [];
        //value.children is an HTML collection, converting to array here
        var childrenArr = Array.prototype.slice.call(element.children);
        //Assumed only one level of children, the <rect> and one or more <text>
        childrenArr.forEach(function(child) {
            var childEntry = {};
            childEntry['attributes'] = getAttributes(child.attributes);
            childEntry['style'] = getStyles(childEntry['attributes']['style']);
            //innerHTML is just for text within the <text>
            //there aren't anymore children since it was assumed only one level of children
            childEntry['innerHTML'] = child.innerHTML;
            entry['children'].push(childEntry);
        });
        dictList.push(entry);
    });
    return dictList;
}

var ReactSVG = React.createClass({
    componentDidMount: function() {
        //Need to hardcode these in because React does not understand these attributes
        var svgNode = React.findDOMNode(this.refs.svg);
        var markerNode = React.findDOMNode(this.refs.marker);
        
        svgNode.setAttribute('xmlns','http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:xlink','http://www.w3.org/1999/xlink');
        svgNode.setAttribute('xmlns:svg','http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:dc','http://purl.org/dc/elements/1.1/');
        svgNode.setAttribute('xmlns:cc','http://creativecommons.org/ns#');
        svgNode.setAttribute('xmlns:rdf','http://www.w3.org/1999/02/22-rdf-syntax-ns#');
        svgNode.setAttribute('version', '1.1');
        
        markerNode.setAttribute('refX', 4);
        markerNode.setAttribute('refY', 5);
        markerNode.setAttribute('markerUnits', 'strokeWidth');
        markerNode.setAttribute('orient', 'auto');
        markerNode.setAttribute('markerWidth', 7);
        markerNode.setAttribute('markerHeight', 7);
        markerNode.setAttribute('viewBox', '0 0 10 10');
    },
    
    nodeClick: function(event) {
        var courseID = event.currentTarget.id;
        var currentNode = this.refs['nodes'].refs[courseID];
        currentNode.turn(this);
    },
    
    nodeMouseEnter: function(event) {
        var courseID = event.currentTarget.id;
        var currentNode = this.refs['nodes'].refs[courseID];
        currentNode.focusPrereqs(this);
        
        //Old hover modal code
        if ($(".modal").length === 0 && activeFocus === '') {
            removeToolTips();
            displayTooltip(courseID);
        }
    },
    
    nodeMouseLeave: function(event) {
        //code here
        var courseID = event.currentTarget.id;
        var currentNode = this.refs['nodes'].refs[courseID];   
        currentNode.unfocusPrereqs(this);
        
        //Old hover modal code
        if ($(".modal").length === 0) {
            var timeout = setTimeout(function () {
                $('.tooltip-group').hide('slow', function () { $(this).remove();});
            }, 100);

            timeouts.push(timeout);
        }
    },
    
    render: function() {
        //not all of these properties are supported in React
        var svgAttrs = {'width': this.props.width, 'height': this.props.height};
        var markerAttrs = {'id': 'arrow'};
        var polylineAttrs = {'points': '0,1 10,5 0,9', 'fill': 'black'};
        return (
            <svg {... svgAttrs} ref='svg'>
                <defs>
                    <marker {... markerAttrs} ref='marker' >
                        <polyline {... polylineAttrs}/>
                    </marker>
                </defs>
                <ReactRegions/>
                <ReactNodes ref='nodes'
                            onClick={this.nodeClick}
                            onMouseEnter={this.nodeMouseEnter}
                            onMouseLeave={this.nodeMouseLeave}
                            svg={this}/>
                <ReactBools ref='bools'/>
                <ReactEdges ref='edges'/>
                <ReactRegionLabels/>
            </svg>
        );
    }
});

var ReactRegionLabels = React.createClass({
    getInitialState: function() {
        return {
            labelsList: []
        };
    },
    
    componentDidMount: function() {
        var dictList = [];
        $('#region-labels > text').map(function(key, element) {
            var entry = {};
            entry['id'] = element.id;
            entry['attributes'] = getAttributes(element.attributes);
            entry['style'] = getStyles(entry['attributes']['style']);
            entry['innerHTML'] = element.innerHTML;
            dictList.push(entry);
        });
        this.setState({labelsList:dictList});
    },
    
    render: function() {
        return (
            <g id='region-labels'>
                {this.state.labelsList.map(function(entry, value) {
                    return <text {... entry['attributes']} key={value} style={entry['style']}>{entry['innerHTML']}</text>
                })}
            </g>
        );
    }
});

var ReactRegions = React.createClass({
    getInitialState: function() {
        return {
            regionsList: []
        };
    },
    
    componentDidMount: function() {
        this.setState({regionsList:getNodes('.region')});
    },
    
    render: function() {
        return (
            <g id='regions'>
                {this.state.regionsList.map(function(entry, value) {
                    return <ReactRegion attributes={entry['attributes']} className='region' key={entry['id']} styles={entry['style']}/>
                })}
            </g>
        );
    }
});

var ReactRegion = React.createClass({
    render: function() {
        //hard-coded className
        return (
            <path {... this.props.attributes} className={this.props.className} style={this.props.styles}>
            </path>
        );
    }
});

var ReactNodes = React.createClass({
    getInitialState: function() {
        return {
            nodesList: [],
            hybridsList: []
        };
    },
    
    componentDidMount: function() {
        this.setState({nodesList:getNodes('.node')});
        this.setState({hybridsList:getNodes('.hybrid')});  
    },
    
    render: function() {
        var svg = this.props.svg;
        return (
            <g id='nodes' stroke='black'>
                {this.state.nodesList.map(function(entry, value) {
                    var parents = [];
                    var childs = [];
                    var outEdges = [];
                    var inEdges = [];

                    $('.path').map(function(key, element) {
                        if (entry['id'] == element.getAttribute('data-target-node')){
                            parents.push(element.getAttribute('data-source-node'));
                            inEdges.push(element.id);
                        }
                        if (entry['id'] == element.getAttribute('data-source-node')){
                            childs.push(element.getAttribute('data-target-node'));
                            outEdges.push(element.id);
                        }
                    });
                    return <ReactNode
                            attributes={entry['attributes']}
                            children={entry['children']}
                            className={'node'}
                            key={entry['id']}
                            styles={entry['style']}
                            hybrid={false}
                            ref={entry['id']}
                            parents={parents}
                            childs={childs}
                            inEdges={inEdges}
                            outEdges={outEdges}
                            {... this.props}
                            svg={svg}
                            hybrid={false}/>
                }, this)}
    
                {this.state.hybridsList.map(function(entry, value) {
                    var parents = [];
                    var childs = [];
                    var outEdges = [];
                    var inEdges = [];

                    $('.path').map(function(key, element) {
                        if (entry['id'] == element.getAttribute('data-target-node')){
                            parents.push(element.getAttribute('data-source-node'));
                            inEdges.push(element.id);
                        }
                        if (entry['id'] == element.getAttribute('data-source-node')){
                            childs.push(element.getAttribute('data-target-node'));
                            outEdges.push(element.id);
                        }
                    });
                    return <ReactNode
                            attributes={entry['attributes']}
                            children={entry['children']}
                            className={'hybrid'}
                            key={entry['id']}
                            styles={entry['style']}
                            hybrid={true}
                            ref={entry['id']}
                            parents={parents}
                            childs={childs}
                            inEdges={inEdges}
                            outEdges={outEdges}
                            svg={svg}
                            hybrid={true}/>
                }, this)}
            </g>
        );
    }
});

var ReactNode = React.createClass({
    getInitialState: function() {
        var id = this.props.attributes['id'];
        var type = 'AND'; //Need to figure out whether it is a OR or AND
        var status = this.props.parents.length == 0 ? 'takeable' : 'inactive';

        return {
            id: id,
            logicalType: type,
            status: status,
        };
    },
    
    isSelected: function () {
        return this.state.status === 'active' || this.state.status === 'overridden';
    },
    
    arePrereqsSatisfied: function(svg) {
        var sat = true;
        function isAllTrue(element, index, array) {
            //use ? operator
            if (svg.refs['nodes'].refs[element]){
                return svg.refs['nodes'].refs[element].isSelected();
            }
            if (svg.refs['bools'].refs[element]){
                return svg.refs['bools'].refs[element].isSelected();
            }
        }
        
        if (this.state.logicalType === 'AND') {
            sat = this.props.parents.every(isAllTrue);     
        } else if (this.state.logicalType === 'OR') {
            sat = this.props.parents.some(isAllTrue);
        }
        return sat;
    },
    
    updateNode: function(svg) {
        var newState =''
        if (this.arePrereqsSatisfied(svg)) {
            if (this.isSelected() || this.props.hybrid) {
                newState='active';
                this.setState({status: 'active'});
            } else {
                newState='takeable';
                this.setState({status: 'takeable'});
            }
        } else {
            if (this.isSelected() && !this.props.hybrid) {
                newState='overridden';
                this.setState({status: 'overridden'});
            } else {
                newState='inactive';
                this.setState({status: 'inactive'});
            }
        }
        
        //setCookie(this.id, this.status);

        // Always update children of hybrids
        if (this.state.hybrid) {
            $.each(this.props.child, function(i, node) {
                var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                 svg.refs['bools'].refs[node];
                currentNode.setState({status: newState},
                                     function(){this.updateNode(svg)}.bind(currentNode));
            });
            $.each(this.props.outEdges, function (i, edge) {
                var currentEdge = svg.refs['edges'].refs[edge];
                currentEdge.setState({status: newState}, 
                                     function(){this.updateEdge(svg)}.bind(currentEdge));
            });
        }

    },
    
    turn: function(svg){
        if (this.isSelected()) {  
            if (this.props.parents.length == 0){
                this.setState({status: 'takeable'}, function(){this.updateNode(svg)}.bind(this));
            } else {
                this.setState({status: 'inactive'}, function(){this.updateNode(svg)}.bind(this));
            }
        } else {
            this.setState({status: 'active'}, function(){this.updateNode(svg)}.bind(this));
        }
                        
        $.each(this.props.childs, function (i, node) {
            var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                             svg.refs['bools'].refs[node];
            currentNode.setState({status: currentNode.state.status},
                                 function(){this.updateNode(svg)}.bind(currentNode));
        });
        
        $.each(this.props.outEdges, function (i, edge) {
            var currentEdge = svg.refs['edges'].refs[edge];
            currentEdge.setState({status: currentEdge.state.status}, 
                                 function(){this.updateEdge(svg)}.bind(currentEdge));
        });
        
        $.each(this.props.inEdges, function (i, edge) {
            var currentEdge = svg.refs['edges'].refs[edge];
            currentEdge.setState({status: currentEdge.state.status}, 
                                 function(){this.updateEdge(svg)}.bind(currentEdge));
        });
    },
    
    focusPrereqs: function(svg){
        if (this.state.status !== 'active') {
            if (this.state.status !== 'overridden') {
                this.setState({status: 'missing'});
            }
            
            $.each(this.props.inEdges, function (i, edge) {
                var currentEdge = svg.refs['edges'].refs[edge];
                var sourceNode = svg.refs['nodes'].refs[currentEdge.props.source] ? svg.refs['nodes'].refs[currentEdge.props.source] :
                                                                                    svg.refs['bools'].refs[currentEdge.props.source];
                if (sourceNode.state.status !== 'active') {
                    currentEdge.setState({status: 'missing'});
                }
            });
            
            $.each(this.props.parents, function (i, node) {
                var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                 svg.refs['bools'].refs[node];
                currentNode.focusPrereqs(svg);
            });
        }
    },
    
    unfocusPrereqs: function(svg){
        if (!this.isSelected()) {
            /*if (activeFocus === '' ||
                window[activeFocus + 'FocusList'].indexOf(this.id) > -1) {
                this.updateSVG();
            } else {
                $('#' + this.id).attr('data-active', 'unlit');
            }*/
            var status = this.props.parents.length == 0 ? 'takeable' : 'inactive';
            this.setState({status: status});
        }

        $.each(this.props.parents, function (i, node) {
            var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                             svg.refs['bools'].refs[node];
            currentNode.unfocusPrereqs(svg);
        });
        $.each(this.props.outEdges, function (i, edge) {
            var currentEdge = svg.refs['edges'].refs[edge];
            currentEdge.setState({status: currentEdge.state.status}, 
                                 function(){this.updateEdge(svg)}.bind(currentEdge));
        });
    },
    
    render: function() {    
        //hard-coded className
        var newClassName = this.props.className;
        if (!this.props.hybrid){
            newClassName += ' ' + this.state.status;
        }
        return (
            <g {... this.props.attributes} style={this.props.styles} {... this.props} className={newClassName}>
                <rect {... this.props.children[0]['attributes']} style={this.props.children[0]['style']}>
                </rect>
                {//this.props.node.children is an HTMLCollection, not an array
                this.props.children.slice(1).map(function(textTag, value) {
                    //hard-coded textAnchor
                    return <text {... textTag['attributes']} key={value} style={textTag['style']} textAnchor='middle'>{textTag['innerHTML']}</text>;
                })}
            </g>
        );
    }
});

var ReactBools = React.createClass({
    getInitialState: function() {
        return {
            boolsList: []
        };
    },
    
    componentDidMount: function() {
        this.setState({boolsList:getNodes('.bool')});
    },
    
    render: function() {
        return (
            <g id='bools'>
                {this.state.boolsList.map(function(entry, value) {
                    var parents = [];
                    var childs = [];
                    var outEdges = [];
                    var inEdges = [];

                    $('.path').map(function(key, element) {
                        if (entry['id'] == element.getAttribute('data-target-node')){
                            parents.push(element.getAttribute('data-source-node'));
                            inEdges.push(element.id);
                        }
                        if (entry['id'] == element.getAttribute('data-source-node')){
                            childs.push(element.getAttribute('data-target-node'));
                            outEdges.push(element.id);
                        }
                    });
                    return <ReactBool attributes={entry['attributes']} children={entry['children']} className='bool' key={entry['id']} styles={entry['style']} ref={entry['id']} ref={entry['id']} parents={parents} childs={childs} inEdges={inEdges} outEdges={outEdges}/>
                })}
            </g>
        );
    }
});

var ReactBool = React.createClass({
    getInitialState: function() {
        var id = this.props.attributes['id'];
        var type = this.props.children[1].innerHTML.toUpperCase();
        var status = 'inactive';
    
        if (this.props.parents.length == 0){
            status = 'takeable';
        }

        return {
            id: id,
            logicalType: type,
            hybrid: this.props.hybrid,
            status: status,
        };
    },
    
    isSelected: function () {
        return this.state.status === 'active' || this.state.status === 'overridden';
    },
    
    arePrereqsSatisfied: function(svg) {
        var sat = true;
        function isAllTrue(element, index, array) {
            return svg.refs['nodes'].refs[element] ? svg.refs['nodes'].refs[element].isSelected() :
                                                     svg.refs['bools'].refs[element].isSelected();
        }
        
        if (this.state.logicalType === 'AND') {
            sat = this.props.parents.every(isAllTrue);     
        } else if (this.state.logicalType === 'OR') {
            sat = this.props.parents.some(isAllTrue);
        }
        return sat;
    },
    
    updateNode: function(svg) {
        var newState =''
        if (this.arePrereqsSatisfied(svg)) {
            if (this.isSelected() || this.props.hybrid) {
                newState='active';
                this.setState({status: 'active'});
            } else {
                newState='takeable';
                this.setState({status: 'takeable'});
            }
        } else {
            if (this.isSelected() && !this.props.hybrid) {
                newState='overridden';
                this.setState({status: 'overridden'});
            } else {
                newState='inactive';
                this.setState({status: 'inactive'});
            }
        }
        
        //setCookie(this.id, this.status);

        // Always update children of hybrids
        if (this.state.hybrid) {
            $.each(this.props.child, function(i, node) {
                var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                 svg.refs['bools'].refs[node];
                currentNode.setState({status: newState},
                                     function(){this.updateNode(svg)}.bind(currentNode));
            });
            $.each(this.props.outEdges, function (i, edge) {
                var currentEdge = svg.refs['edges'].refs[edge];
                currentEdge.setState({status: newState}, 
                                     function(){this.updateEdge(svg)}.bind(currentEdge));
            });
        }

    },
    
    focusPrereqs: function(svg){
        if (this.state.status !== 'active') {
            if (this.state.status !== 'overridden') {
                this.setState({status: 'missing'});
            }
            
            $.each(this.props.inEdges, function (i, edge) {
                var currentEdge = svg.refs['edges'].refs[edge];
                var sourceNode = svg.refs['nodes'].refs[currentEdge.props.source] ? svg.refs['nodes'].refs[currentEdge.props.source] :
                                                                                    svg.refs['bools'].refs[currentEdge.props.source];
                if (sourceNode.state.status !== 'active') {
                    currentEdge.setState({status: 'missing'});
                }
            });
            
            $.each(this.props.parents, function (i, node) {
                var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                 svg.refs['bools'].refs[node];
                currentNode.focusPrereqs(svg);
            });
        }
    },
    
    unfocusPrereqs: function(svg){
        if (!this.isSelected()) {
            /*if (activeFocus === '' ||
                window[activeFocus + 'FocusList'].indexOf(this.id) > -1) {
                this.updateSVG();
            } else {
                $('#' + this.id).attr('data-active', 'unlit');
            }*/
            var status = this.props.parents.length == 0 ? 'takeable' : 'inactive';
            this.setState({status: status});
        }

        $.each(this.props.parents, function (i, node) {
            var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                             svg.refs['bools'].refs[node];
            currentNode.unfocusPrereqs(svg);
        });
        $.each(this.props.outEdges, function (i, edge) {
            var currentEdge = svg.refs['edges'].refs[edge]
            currentEdge.setState({status: currentEdge.state.status}, 
                                 function(){this.updateEdge(svg, currentEdge.state.status)}.bind(currentEdge));
        });
    },

    render: function() {
        //hard-coded className
        //All bools start as inactive, will need to not hardcode this later
        var newClassName = this.props.className + ' ' + this.state.status;
        return (
            <g className={newClassName} {... this.props.attributes} style={this.props.styles}>
                <ellipse {... this.props.children[0]['attributes']} style={this.props.children[0]['style']}>
                </ellipse>
                {//this.props.node.children is an HTMLCollection, not an array
                this.props.children.slice(1).map(function(textTag, value) {
                    //hard-coded textAnchor
                    return <text {... textTag['attributes']} key={value} style={textTag['style']} textAnchor='middle'>{textTag['innerHTML']}</text>;
                })}
            </g>
        );
    }
});


var ReactEdges = React.createClass({
    getInitialState: function() {
        return {
            edgesList: []
        };
    },
    
    componentDidMount: function() {
        this.setState({edgesList:getNodes('.path')});
    },
    
    render: function() {
        return (
            <g id='edges' stroke='black'>
                {this.state.edgesList.map(function(entry, value) {
                    return <ReactEdge
                            attributes={entry['attributes']}
                            className='path'
                            key={entry['id']}
                            styles={entry['style']}
                            ref={entry['id']}
                            source={entry['attributes']['data-source-node']}
                            target={entry['attributes']['data-target-node']}/>
                })}
            </g>
        );
    }
});

var ReactEdge = React.createClass({
    getInitialState: function() {
        var status = 'inactive';
        var id = this.props.attributes['id'];
        return {
            id: id,
            status: status
        };
    },

    updateEdge: function(svg) {
        sourceNode = svg.refs['nodes'].refs[this.props.source] ? svg.refs['nodes'].refs[this.props.source] :
                                                                 svg.refs['bools'].refs[this.props.source];
        targetNode = svg.refs['nodes'].refs[this.props.target] ? svg.refs['nodes'].refs[this.props.target] :
                                                                 svg.refs['bools'].refs[this.props.target];
        if (!sourceNode.isSelected()) {
            this.setState({status: 'inactive'});
        } else if (!targetNode.isSelected()) {
            this.setState({status: 'takeable'});
        } else {
            this.setState({status: 'active'});
        }
    },
    
    render: function() {
        //hard-coded className and markerEnd
        //All edges start as inactive, will need to not hardcode this later
        var newClassName = this.props.className + ' ' + this.state.status;
        return (
            <path {... this.props.attributes} className={newClassName} style={this.props.styles} markerEnd='url(#arrow)'>
            </path>
        );
    }
});
