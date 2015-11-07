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
    componentDidMount: function(){
        //Need to hardcode these in because React does not understand these attributes
        var svgNode = React.findDOMNode(this.refs.svg);
        var markerNode = React.findDOMNode(this.refs.marker);
        
        svgNode.setAttribute('xmlns','http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:xlink','http://www.w3.org/1999/xlink');
        svgNode.setAttribute('xmlns:svg','http://www.w3.org/2000/svg');
        svgNode.setAttribute('xmlns:dc','http://purl.org/dc/elements/1.1/');
        svgNode.setAttribute('xmlns:cc','http://creativecommons.org/ns#');
        svgNode.setAttribute('xmlns:rdf','http://www.w3.org/1999/02/22-rdf-syntax-ns#');
        
        markerNode.setAttribute('viewBox', '0 0 10 10');
        markerNode.setAttribute('refX', 4);
        markerNode.setAttribute('refY', 5);
        markerNode.setAttribute('markerUnits', 'strokeWidth');
        markerNode.setAttribute('orient', 'auto');
        markerNode.setAttribute('markerWidth', 7);
        markerNode.setAttribute('markerHeight', 7);
    },
    render: function() {
        //not all of these properties are supported in React
        var svgAttrs = {'width':this.props.width, 'height':this.props.height, 'version':'1.1'};
        var markerAttrs = {'id':'arrow'};
        var polylineAttrs = {'points':'0,1 10,5 0,9', 'fill':'black'};
        return (
            <svg {... svgAttrs} ref ='svg'>
                <defs>
                    <marker {... markerAttrs} ref='marker' >
                        <polyline {... polylineAttrs}/>
                    </marker>
                </defs>
                <ReactRegions/>
                <ReactNodes/>
                <ReactBools/>
                <ReactEdges/>
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
        return (
            <g id='nodes' stroke='black'>
                {this.state.nodesList.map(function(entry, value) {
                    return <ReactNode attributes={entry['attributes']} children={entry['children']} className='node' key={entry['id']} styles={entry['style']}/>
                })}
                {this.state.hybridsList.map(function(entry, value) {
                    return <ReactNode attributes={entry['attributes']} children={entry['children']} className='hybrid' key={entry['id']} styles={entry['style']}/>
                })}
            </g>
        );
    }
});

var ReactNode = React.createClass({
    render: function() {
        //Quick fix to initiallyTakeable
        //Will later remove initiallyTakeable, by making a check if prerequisite list is empty or not
        var initiallyTakeableUpper = $.map(initiallyTakeable, function(item, index) {
            return item.toUpperCase();
        });
        
        if (initiallyTakeableUpper.indexOf(this.props.attributes['id'].toUpperCase()) > -1) {
            this.props.attributes['data-active'] = 'takeable';
        } else {
            this.props.attributes['data-active'] = 'inactive';
        }
        //hard-coded className
        return (
            <g className={this.props.className} {... this.props.attributes} style={this.props.styles}>
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
                    return <ReactBool attributes={entry['attributes']} children={entry['children']} className='bool' key={entry['id']} styles={entry['style']}/>
                })}
            </g>
        );
    }
});

var ReactBool = React.createClass({
    render: function() {
        //hard-coded className
        //All bools start as inactive, will need to not hardcode this later
        this.props.attributes['data-active'] = 'inactive';
        return (
            <g className={this.props.className} {... this.props.attributes} style={this.props.styles}>
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
                    return <ReactEdge attributes={entry['attributes']} className='path' key={entry['id']} styles={entry['style']}/>
                })}
            </g>
        );
    }
});

var ReactEdge = React.createClass({
    render: function() {
        //hard-coded className and markerEnd
        //All edges start as inactive, will need to not hardcode this later
        this.props.attributes['data-active'] = 'inactive';
        return (
            <path {... this.props.attributes} className={this.props.className} style={this.props.styles} markerEnd='url(#arrow)'>
            </path>
        );
    }
});
