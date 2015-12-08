function renderReactGraph() {
    'use strict';
    React.render(
        <ReactSVG width={1195} height={650}/>,
        document.getElementById('react-graph')
    );
}

var ReactSVG = React.createClass({
    getInitialState: function () {
        return {
            labelsJSON: [],
            regionsJSON: [],
            nodesJSON: [],
            hybridsJSON: [],
            boolsJSON: [],
            edgesJSON: []
        };
    },
    componentDidMount: function () {
        $.ajax({
            dataType: 'json',
            url: 'graph-json',
            success: function (data) {
                var labelsList = [];
                var regionsList = [];
                var nodesList = [];
                var hybridsList = [];
                var boolsList = [];
                var edgesList = [];
                
                //data[0] is ["texts", [JSON]]
                data[0][1].forEach(function (entry) {
                    if (entry['rId'].substring(0,5) === 'tspan'){
                        labelsList.push(entry);
                    }
                });
                //data[1] is ["shapes", [JSON]]
                data[1][1].forEach(function (entry) {
                    if (entry['type_'] === 'Node'){
                        nodesList.push(entry);
                    }
                    if (entry['type_'] === 'Hybrid'){
                        hybridsList.push(entry);
                    }
                    if (entry['type_'] === 'BoolNode'){
                        boolsList.push(entry);
                    }
                });
                //data[2] is ["paths", [JSON]]
                //data[2][1] are the JSON without "paths"
                data[2][1].forEach(function (entry) {
                    if (entry['isRegion']){
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

    nodeClick: function (event) {
        var courseID = event.currentTarget.id;
        var currentNode = this.refs['nodes'].refs[courseID];
        currentNode.toggleSelection(this);
    },

    nodeMouseEnter: function (event) {
        var courseID = event.currentTarget.id;
        var currentNode = this.refs['nodes'].refs[courseID];
        currentNode.focusPrereqs(this);

        //Old hover modal code
        if ($(".modal").length === 0) {
            removeToolTips();
            displayTooltip(courseID);
        }
    },

    nodeMouseLeave: function (event) {
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

    render: function () {
        //not all of these properties are supported in React
        var svgAttrs = {'width': this.props.width, 'height': this.props.height};
        var markerAttrs = {'id': 'arrow'};
        var polylineAttrs = {'points': '0,1 10,5 0,9', 'fill': 'black'};
        return (
            <svg {... svgAttrs} ref='svg'>
                <defs>
                    <marker {... markerAttrs} ref='marker'>
                        <polyline {... polylineAttrs}/>
                    </marker>
                </defs>
                <ReactRegions regionsJSON={this.state.regionsJSON}/>
                <ReactNodes ref='nodes'
                            onClick={this.nodeClick}
                            onMouseEnter={this.nodeMouseEnter}
                            onMouseLeave={this.nodeMouseLeave}
                            svg={this}
                            nodesJSON={this.state.nodesJSON}
                            hybridsJSON={this.state.hybridsJSON}/>
                <ReactBools ref='bools'
                            boolsJSON={this.state.boolsJSON}/>
                <ReactEdges ref='edges'
                            edgesJSON={this.state.edgesJSON}s/>
                <ReactRegionLabels labelsJSON={this.state.labelsJSON}/>
            </svg>
        );
    }
});

var ReactRegionLabels = React.createClass({
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

var ReactRegions = React.createClass({
    render: function () {
        return (
            <g id='regions'>
                {this.props.regionsJSON.map(function (entry, value) {
                    return <ReactRegion
                            key={value}
                            JSON={entry}
                            className='region'
                            />
                },this)}
            </g>
        );
    }
});

var ReactRegion = React.createClass({
    render: function () {
        var pathAttrs = {};
        pathAttrs['d'] = 'M';
        //Is there a better way to do this?
        this.props.JSON.points.forEach(function(x){
            pathAttrs['d'] += x[0] + ',' + x[1] + ' '});
        
        var pathStyle = {
            fill : this.props.JSON.fill
        }
        
        return (
            <path {... pathAttrs} className={this.props.className} style={pathStyle}>
            </path>
        );
    }
});

var ReactNodes = React.createClass({
    render: function () {
        return (
            <g id='nodes' stroke='black'>
                {this.props.nodesJSON.map(function (entry, value) {
                    return <ReactNode
                            {...this.props}
                            key={value}
                            JSON={entry}
                            hybrid={false}
                            logicalType='AND'
                            className='node'
                            parents={[]}
                            childs={[]}
                            outEdges={[]}
                            inEdges={[]}
                            />
                },this)}
                {this.props.hybridsJSON.map(function (entry, value) {
                    return <ReactNode
                            {...this.props}
                            key={value}
                            JSON={entry}
                            hybrid={true}
                            logicalType='AND'
                            className='hybrid'
                            parents={[]}
                            childs={[]}
                            outEdges={[]}
                            inEdges={[]}
                            />
                },this)}
            </g>
        );
    }
});

var ReactNode = React.createClass({
    getInitialState: function () {
        return {
            status: this.props.parents.length === 0 ? 'takeable' : 'inactive',
        };
    },

    isSelected: function () {
        return this.state.status === 'active' || this.state.status === 'overridden';
    },

    arePrereqsSatisfied: function (svg) {
        function isAllTrue(element, index, array) {
            return svg.refs['nodes'].refs[element] ? svg.refs['nodes'].refs[element].isSelected() :
                                                     svg.refs['bools'].refs[element].isSelected();
        }

        if (this.props.logicalType === 'AND') {
            return this.props.parents.every(isAllTrue);     
        } else if (this.props.logicalType === 'OR') {
            return this.props.parents.some(isAllTrue);
        }
    },
    
    updateNode: function (svg) {
        var newState = this.arePrereqsSatisfied(svg) ? (this.isSelected() || this.props.hybrid) ? 'active' : 'takeable' :
                                               (this.isSelected() && !this.props.hybrid) ? 'overridden' : 'inactive';

        this.setState({status: newState},
                      function () {
                          // Always update children of hybrids
                          if (this.props.hybrid) {
                              this.props.childs.forEach(function (node, i) {
                                  var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                                   svg.refs['bools'].refs[node];
                                  currentNode.updateNode(svg);
                              });
                              this.props.outEdges.forEach(function (edge, i) {
                                  var currentEdge = svg.refs['edges'].refs[edge];
                                  currentEdge.updateEdge(svg);
                              });
                          }
                      }
        );
    },

    toggleSelection: function (svg) {
        var newStatus = this.isSelected() ? (this.props.parents.length === 0 ? 'takeable' : 'inactive') : 'active';

        this.setState({status: newStatus}, 
                      function () { 
                          this.updateNode(svg);
                          this.props.childs.forEach(function (node, i) {
                              var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                               svg.refs['bools'].refs[node];
                              currentNode.updateNode(svg);
                          });
                          this.props.outEdges.forEach(function (edge, i) {
                              var currentEdge = svg.refs['edges'].refs[edge];
                              currentEdge.updateEdge(svg);
                          });
                          this.props.inEdges.forEach(function (edge, i) {
                              var currentEdge = svg.refs['edges'].refs[edge];
                              currentEdge.updateEdge(svg);
                          });
                      }
        );
    },

    focusPrereqs: function (svg) {
        if (this.state.status !== 'active') {
            if (this.state.status !== 'overridden') {
                this.setState({status: 'missing'},
                              function () {
                                  this.props.inEdges.forEach(function (edge, i) {
                                      var currentEdge = svg.refs['edges'].refs[edge];
                                      var sourceNode = svg.refs['nodes'].refs[currentEdge.props.source] ?
                                                       svg.refs['nodes'].refs[currentEdge.props.source] :
                                                       svg.refs['bools'].refs[currentEdge.props.source];
                                      if (sourceNode.state.status !== 'active') {
                                          currentEdge.setState({status: 'missing'});
                                      }
                                  });
                                  this.props.parents.forEach(function (node, i) {
                                      var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                                       svg.refs['bools'].refs[node];
                                      currentNode.focusPrereqs(svg);
                                  });
                              }
                );
            }
        }
    },

    unfocusPrereqs: function (svg) {
        var status = this.props.parents.length === 0 ? 'takeable' : 'inactive';
        if (!this.isSelected()) {
            this.setState({status: status},
                          function () {
                              this.props.outEdges.forEach(function (edge, i) {
                                  var currentEdge = svg.refs['edges'].refs[edge];
                                  currentEdge.updateEdge(svg);
                              });
                          }
            );
        }
        this.props.parents.forEach(function (node, i) {
          var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                           svg.refs['bools'].refs[node];
          currentNode.unfocusPrereqs(svg);
        });
    },

    render: function () {
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
        
        return (
            <g {... this.props} {... gAttrs} className={!this.props.hybrid ? this.props.className + ' ' + this.state.status : this.props.className} >
                <rect {... rectAttrs} style={rectStyle}>
                </rect>
                <text>
                </text>
            </g>
        );
    }
    
});

var ReactBools = React.createClass({
    render: function () {
        return (
            <g id='bools'>
                {this.props.boolsJSON.map(function (entry, value) {
                    return <ReactBool
                            {... this.props}
                            key={value}
                            JSON={entry}
                            hybrid={true}
                            logicalType='AND/OR'
                            className='bool'
                            parents={[]}
                            childs={[]}
                            outEdges={[]}
                            inEdges={[]}
                            />
                },this)}
            </g>
        );
    }
});

var ReactBool = React.createClass({
    getInitialState: function () {
        return {
            status: 'inactive'
        };
    },

    isSelected: function () {
        return this.state.status === 'active' || this.state.status === 'overridden';
    },

    arePrereqsSatisfied: function (svg) {
        function isAllTrue(element, index, array) {
            return svg.refs['nodes'].refs[element] ? svg.refs['nodes'].refs[element].isSelected() :
                                                     svg.refs['bools'].refs[element].isSelected();
        }

        if (this.props.logicalType === 'AND') {
            return this.props.parents.every(isAllTrue);     
        } else if (this.props.logicalType === 'OR') {
            return this.props.parents.some(isAllTrue);
        }
    },

    updateNode: function (svg) {
        var newState = this.arePrereqsSatisfied(svg) ? (this.isSelected() || this.props.hybrid) ? 'active' : 'takeable' :
                                                       (this.isSelected() && !this.props.hybrid) ? 'overridden' : 'inactive';
        this.setState({status: newState},
                      function () {
                          // Always update children of hybrids
                          if (this.props.hybrid) {
                              this.props.childs.forEach(function (node, i) {
                                  var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                                   svg.refs['bools'].refs[node];
                                  currentNode.updateNode(svg);
                              });
                              this.props.outEdges.forEach(function (edge, i) {
                                  var currentEdge = svg.refs['edges'].refs[edge];
                                  currentEdge.updateEdge(svg);
                              });
                          }
                      }
        );
    },

    focusPrereqs: function (svg) {
        if (this.state.status !== 'active') {
            if (this.state.status !== 'overridden') {
                this.setState({status: 'missing'},
                              function () {
                                  this.props.inEdges.forEach(function (edge, i) {
                                      var currentEdge = svg.refs['edges'].refs[edge];
                                      var sourceNode = svg.refs['nodes'].refs[currentEdge.props.source] ?
                                                       svg.refs['nodes'].refs[currentEdge.props.source] :
                                                       svg.refs['bools'].refs[currentEdge.props.source];
                                      if (sourceNode.state.status !== 'active') {
                                          currentEdge.setState({status: 'missing'});
                                      }
                                  });
                                  this.props.parents.forEach(function (node, i) {
                                      var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
                                                                                       svg.refs['bools'].refs[node];
                                      currentNode.focusPrereqs(svg);
                                  });
                              }
                );
            }
        }
    },

    unfocusPrereqs: function (svg) {
        var status = 'inactive';
        if (!this.isSelected()) {
            this.setState({status: status},
                          function () {
                              this.props.outEdges.forEach(function (edge, i) {
                                  var currentEdge = svg.refs['edges'].refs[edge];
                                  currentEdge.updateEdge(svg);
                              });
                          }
            );
        }
        this.props.parents.forEach(function (node, i) {
          var currentNode = svg.refs['nodes'].refs[node] ? svg.refs['nodes'].refs[node] :
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
            <g {... this.props} className={this.props.className + ' ' + this.state.status} >
                <ellipse {... ellipseAttrs}>
                </ellipse>
                <text>
                </text>
            </g>
        );
    }
});

var ReactEdges = React.createClass({
    render: function () {
        return (
            <g id='edges' stroke='black'>
                {this.props.edgesJSON.map(function (entry, value) {
                    return <ReactEdge
                            key={value}
                            JSON={entry}
                            className='edge'
                            source=''
                            target=''
                            />
                },this)}
            </g>
        );
    }
});

var ReactEdge = React.createClass({
    getInitialState: function () {
        return {
            status: 'inactive'
        };
    },

    updateEdge: function (svg) {
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

    render: function () {
        var pathAttrs = {};
        pathAttrs['d'] = 'M';
        //Is there a better way to do this?
        this.props.JSON.points.forEach(function(x){
            pathAttrs['d'] += x[0] + ',' + x[1] + ' '});
        
        return (
            <path {... pathAttrs} className={this.props.className + ' ' + this.state.status} markerEnd='url(#arrow)'>
            </path>
        );
    }
});
