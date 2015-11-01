function renderReactGraph() {
    React.render(
        <ReactSVG width={1195} height={650}/>,
        document.getElementById('react-graph')
    );
}

function getAttributes(svgAttributes) {
    var attrs = [];
    //Traversing a NodeNamedMap type
    Array.prototype.slice.call(svgAttributes).forEach(function(item) {
        attrs[item.name] = item.value;
    });
    return attrs;
}

function getStyles(styles_strings) {
    var styles = {};
    if (!styles_strings) {
        return styles;
    }
    styles_strings.split(";").map(function(key, value){
        if (key) {
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
            nodes_list: []
        };
    },
    
    componentDidMount: function(){
        var dict_list = [];
        
        $('.node').map(function(key, value) {
            var entry = {};
            entry["id"] = value.id;
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
                entry["children"].push(child_entry);
            });
            dict_list.push(entry);
        });
        this.setState({nodes_list:dict_list});
        //LATER: Add AJAX code to pull code here
        //long run, remove SVG generator
        //make a list of dictionarys instead of svgelements
        //because after getting new graph, 
        //attrs are stored in db
    },
    
    render: function() {
        return (
            <g id='nodes'>
                {this.state.nodes_list.map(function(entry, value) {
                    return <ReactNode key={entry["id"]} attributes={entry["attributes"]} style={entry["style"]} children={entry["children"]}/>
                })}
            </g>
        );
    }
});

//Kept as separate component in case it may be needed later
var ReactNode = React.createClass({
    render: function(){
        return (
            <g className='node' {... this.props.attributes} className={this.props.attributes["class"]} style={this.props.styles}>
                <rect {... this.props.children[0]["attributes"]} style={this.props.children[0]["style"]}>
                </rect>
                {//this.props.node.children is an HTMLCollection, not an array
                this.props.children.slice(1).map(function(text_tag, value) {
                    return <text key={value} {... text_tag["attributes"]}>{text_tag["innerHTML"]}</text>;
                })}
            </g>
        );
    }
});
