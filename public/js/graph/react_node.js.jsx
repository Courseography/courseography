function renderReactGraph() {
    React.render(
        <ReactSVG width={1195} height={650}/>,
        document.getElementById('react-graph')
    );
}

function getAttributes(node_named_map) {
    var attrs = [];
    //Traversing a NodeNamedMap type
    Array.prototype.slice.call(node_named_map).forEach(function(item) {
        attrs[item.name] = item.value;
    });
    return attrs;
}

function getStyles(styles_strings) {
    if (!styles_strings){
        return {};
    }
    var styles = {};
        styles_strings.split(";").map(function(key, value){
        if (key){
            styles[key.substring(0, key.indexOf(':'))] = key.substring(key.indexOf(':')+1);
        }
    });
    return styles;
}

var ReactSVG = React.createClass({
    render: function() {
        return (
            <svg width={this.props.width} height={this.props.height}>
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
            nodes_list: [],
            nodes_list_dict: []
        };
    },
    
    componentDidMount: function(){
        var arr = [];
        var dict_list = [];
        
      
        $('.node').map(function(key, value) {
            arr.push(value);
          
            var entry = {};
            entry["attributes"] = getAttributes(value.attributes);
            //assume no styles here
            entry["style"] = getStyles(entry["attributes"]["style"]);
            entry["children"] = [];
            //value.children is an HTML collection, converting to array here
            var children_arr = [].slice.call(value.children);
            //assume only one level of children
            //for each children
            children_arr.map(function(child) {
                var child_entry = {};
                child_entry["attributes"] = getAttributes(child.attributes);
                child_entry["style"] = getStyles(child_entry["attributes"]["style"]);
                //innerHTML is just for the text tag
                child_entry["innerHTML"] = child.innerHTML;
                //console.log(child_entry["style"]);
                entry["children"].push(child_entry);
            });
            dict_list.push(entry);
          
        });
        this.setState({nodes_list:arr});
        this.setState({nodes_list_dict:dict_list});
        //LATER: Add AJAX code to pull code here
        //long run, remove SVG generator
        //make a list of dictionarys instead of svgelements
        //because after getting new graph, 
        //attrs are stored in db
        
        
    },
    
    render: function() {
        return (
            <g id='nodes'>
                {/*this.state.nodes_list.map(function(svg_element, value) {
                    return <ReactNode key={value} node={svg_element} node_list/>
                })*/}
                {this.state.nodes_list_dict.map(function(entry, value) {
                    //console.log(entry);
                    return <ReactNodeDict attributes={entry["attributes"]} style={entry["style"]} children={entry["children"]}/>
                })}
                
            </g>
        );
    }
});


var ReactNodeDict = React.createClass({
    render: function(){
        return (
            <g className='node' {... this.props.attributes} style={this.props.styles}>
                <rect {... this.props.children[0]["attributes"]} style={this.props.children[0]["style"]}>
                </rect>
                {//this.props.node.children is an HTMLCollection, not an array
                this.props.children.slice(1).map(function(text_tag) {
                    return <text {... text_tag["attributes"]}>{text_tag["innerHTML"]}</text>;
                })
                }
            </g>
        );
    }
});

//Modify
var ReactNode = React.createClass({
    getInitialState: function(){
        //don't need anything here
        //do everything from props
        return {
            g_attributes: [],
            g_styles: {},
            rect_attributes: [],
            rect_styles: {},
            text_attributes: []
        };
    },
    
    componentDidMount: function(){
        //don't need anything here
        var attrs = getAttributes(this.props.node.attributes);
        this.setState({g_attributes: attrs});
        var styles = getStyles(attrs["style"]);
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
                {//this.props.node.children is an HTMLCollection, not an array
                Array.prototype.slice.call(this.props.node.children).slice(1).map(function(svg_element) {
                    return <text {... getAttributes(svg_element.attributes)}>{svg_element.innerHTML}</text>;
                })
                }
            </g>
        );
    }
});