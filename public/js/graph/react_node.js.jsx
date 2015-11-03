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
    var dict_list = [];
    $(mode).map(function(key, element) {
        var entry = {};
        entry['id'] = element.id;
        entry['attributes'] = getAttributes(element.attributes);
        //<g> themselves currently have no styles, only the <rect> child has styles.
        //The line below returns an empty Object, this is in case we were to add styles later on
        entry['style'] = getStyles(entry['attributes']['style']);
        entry['children'] = [];
        //value.children is an HTML collection, converting to array here
        var children_arr = Array.prototype.slice.call(element.children);
        //Assumed only one level of children, the <rect> and one or more <text>
        children_arr.forEach(function(child) {
            var child_entry = {};
            child_entry['attributes'] = getAttributes(child.attributes);
            child_entry['style'] = getStyles(child_entry['attributes']['style']);
            //innerHTML is just for text within the <text>
            //there aren't anymore children since it was assumed only one level of children
            child_entry['innerHTML'] = child.innerHTML;
            entry['children'].push(child_entry);
        });
        dict_list.push(entry);
    });
    return dict_list;
}

var ReactSVG = React.createClass({
    render: function() {
        return (
            <svg width={this.props.width} height={this.props.height}>
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
            labels_list: []
        };
    },
    
    componentDidMount: function() {
        var dict_list = [];
        $('#region-labels > text').map(function(key, element) {
            var entry = {};
            entry['id'] = element.id;
            entry['attributes'] = getAttributes(element.attributes);
            entry['style'] = getStyles(entry['attributes']['style']);
            entry['innerHTML'] = element.innerHTML;
            dict_list.push(entry);
        });
        this.setState({labels_list:dict_list});
    },
    
    render: function() {
        return (
            <g id='region-labels'>
                {this.state.labels_list.map(function(entry, value) {
                    return <text key={value} {... entry['attributes']} style={entry['style']}>{entry['innerHTML']}</text>
                })}
            </g>
        );
    }
});

var ReactRegions = React.createClass({
    getInitialState: function() {
        return {
            regions_list: []
        };
    },
    
    componentDidMount: function() {
        this.setState({regions_list:getNodes('.region')});
    },
    
    render: function() {
        return (
            <g id='regions'>
                {this.state.regions_list.map(function(entry, value) {
                    return <ReactRegion className='region' key={entry['id']} attributes={entry['attributes']} styles={entry['style']}/>
                })}
            </g>
        );
    }
});

var ReactRegion = React.createClass({
    render: function() {
        //hard-coded className
        return (
            <path className={this.props.className} {... this.props.attributes} style={this.props.styles}>
            </path>
        );
    }
});


var ReactNodes = React.createClass({
    getInitialState: function() {
        return {
            nodes_list: [],
            hybrids_list: []
        };
    },
    
    componentDidMount: function() {
        this.setState({nodes_list:getNodes('.node')});
        this.setState({hybrids_list:getNodes('.hybrid')});
    },
    
    render: function() {
        return (
            <g id='nodes' stroke='black'>
                {this.state.nodes_list.map(function(entry, value) {
                    entry['attributes']['data-active'] = 'inactive';
                    return <ReactNode className='node' key={entry['id']} attributes={entry['attributes']} styles={entry['style']} children={entry['children']}/>
                })}
                {this.state.hybrids_list.map(function(entry, value) {
                    entry['attributes']['data-active'] = 'inactive';
                    return <ReactNode className='hybrid' key={entry['id']} attributes={entry['attributes']} styles={entry['style']} children={entry['children']}/>
                })}
            </g>
        );
    }
});

//Kept as separate component in case it may be needed later
var ReactNode = React.createClass({
    render: function() {
        //hard-coded className
        return (
            <g className={this.props.className} {... this.props.attributes} style={this.props.styles}>
                <rect {... this.props.children[0]['attributes']} style={this.props.children[0]['style']}>
                </rect>
                {//this.props.node.children is an HTMLCollection, not an array
                this.props.children.slice(1).map(function(textTag, value) {
                    //hard-coded textAnchor
                    return <text key={value} textAnchor='middle' {... textTag['attributes']} style={textTag['style']}>{textTag['innerHTML']}</text>;
                })}
            </g>
        );
    }
});

var ReactBools = React.createClass({
    getInitialState: function() {
        return {
            bools_list: []
        };
    },
    
    componentDidMount: function() {
        this.setState({bools_list:getNodes('.bool')});
    },
    
    render: function() {
        return (
            <g id='bools'>
                {this.state.bools_list.map(function(entry, value) {
                    return <ReactBool className='bool' key={entry['id']} attributes={entry['attributes']} styles={entry['style']} children={entry['children']}/>
                })}
            </g>
        );
    }
});

var ReactBool = React.createClass({
    render: function() {
        //hard-coded className
        this.props.attributes['data-active'] = 'inactive';
        return (
            <g className={this.props.className} {... this.props.attributes} style={this.props.styles}>
                <ellipse {... this.props.children[0]['attributes']} style={this.props.children[0]['style']}>
                </ellipse>
                {//this.props.node.children is an HTMLCollection, not an array
                this.props.children.slice(1).map(function(textTag, value) {
                    //hard-coded textAnchor
                    return <text key={value} textAnchor='middle' {... textTag['attributes']} style={textTag['style']}>{textTag['innerHTML']}</text>;
                })}
            </g>
        );
    }
});


var ReactEdges = React.createClass({
    getInitialState: function() {
        return {
            edges_list: []
        };
    },
    
    componentDidMount: function() {
        this.setState({edges_list:getNodes('.path')});
    },
    
    render: function() {
        return (
            <g id='edges' stroke='black'>
                {this.state.edges_list.map(function(entry, value) {
                    entry['attributes']['data-active'] = 'inactive';
                    return <ReactEdge className='path' key={entry['id']} attributes={entry['attributes']} styles={entry['style']}/>
                })}
            </g>
        );
    }
});

var ReactEdge = React.createClass({
    render: function() {
        //hard-coded className and markerEnd
        return (
            <path className={this.props.className} {... this.props.attributes} style={this.props.styles} markerEnd='url(#arrow)'>
            </path>
        );
    }
});
