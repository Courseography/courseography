function renderReactGraph() {
    React.render(
        <ReactSVG />,
        document.getElementById('react-graph')
    );
}

function getAttributes(node_named_map) {
    attrs = [];
    //Traversing a NodeNamedMap type
    Array.prototype.slice.call(node_named_map).forEach(function(item) {
        attrs[item.name] = item.value;
    });
    return attrs;
}

function getStyles(styles_strings) {
    styles = {};
    individual_styles= styles_strings.split(";");
    individual_styles.map(function(key, value){
        if (key){
            styles[key.substring(0, key.indexOf(':'))] = key.substring(key.indexOf(':')+1);
        }
    });
    return styles;
}

var ReactSVG = React.createClass({
    render: function() {
        return (
            <svg width="1195" height="650">
                {/*<ReactRegions/>*/}
                <ReactNodes/>
                {/*<ReactRegions/>*/}
                {/*<ReactBools/>*/}
                {/*<ReactEdges/>*/}
                {/*<ReactRegionLabels/>*/}
            </svg>
        );
    }
});

var ReactNodes = React.createClass({
    getInitialState: function(){
        return {
            nodes_list: []
        };
    },
    
    componentDidMount: function(){
        arr = [];
        $('.node').map(function(key, value) {
            arr.push(value);
        });
        this.setState({nodes_list:arr});
    },
    
    render: function() {
        return (
            <g id='nodes'>
                {this.state.nodes_list.map(function(svg_element, value) {
                    return <ReactNode key={value} node={svg_element}/>
                })}
            </g>
        );
    }
});


var ReactNode = React.createClass({
    getInitialState: function(){
        return {
            g_attributes: [],
            g_styles: {},
            rect_attributes: [],
            rect_styles: {},
            text_attributes: []
        };
    },
    
    componentDidMount: function(){
        attrs = getAttributes(this.props.node.attributes);
        this.setState({g_attributes: attrs});
        styles = getStyles(attrs["style"]);
        this.setState({g_styles: styles});
        
        attrs = getAttributes(this.props.node.children[0].attributes);
        this.setState({rect_attributes: attrs});
        styles = getStyles(attrs["style"]);
        this.setState({rect_styles: styles});
        
        attrs = getAttributes(this.props.node.children[1].attributes);
        this.setState({text_attributes: attrs});
    },
    
    render: function() {
        return (
            <g className='node' {... this.state.g_attributes} style={this.state.g_styles} >
                <rect {... this.state.rect_attributes} style={this.state.rect_styles}>
                </rect>
                <text {... this.state.text_attributes}>
                    {this.props.node.children[1].innerHTML}
                </text>
            </g>
        );
    }
});