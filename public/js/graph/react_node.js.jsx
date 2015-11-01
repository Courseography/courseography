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
        //Will be hard-coding in className and textAnchor
        if (item.name!='class' && item.name!='text-anchor'){
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
    getInitialState: function() {
        return {
            nodes_list: []
        };
    },
    
    componentDidMount: function() {
        var dict_list = [];
        
        $('.node').map(function(key, element) {
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
        this.setState({nodes_list:dict_list});
        //LATER: Add AJAX code to pull code here
        //In the long run, remove SVGGenerator
    },
    
    render: function() {
        return (
            <g id='nodes'>
                {this.state.nodes_list.map(function(entry, value) {
                    return <ReactNode key={entry['id']} attributes={entry['attributes']} style={entry['style']} children={entry['children']}/>
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
            <g className='node' {... this.props.attributes} style={this.props.styles}>
                <rect {... this.props.children[0]['attributes']} style={this.props.children[0]['style']}>
                </rect>
                {//this.props.node.children is an HTMLCollection, not an array
                this.props.children.slice(1).map(function(text_tag, value) {
                    //hard-coded textAnchor
                    return <text key={value} textAnchor='middle' {... text_tag['attributes']} style={text_tag['style']}>{text_tag['innerHTML']}</text>;
                })}
            </g>
        );
    }
});
