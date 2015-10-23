function foo(){
    React.render(
        <ReactSVG />,
        document.getElementById('react-graph')
    );
}

var ReactSVG = React.createClass({
    render: function() {
        return (
            <svg width="1195" height="650">
                <ReactNodes/>
            </svg>
        );
    }
});

//come up with better variable names
var ReactNodes = React.createClass({
    getInitialState: function(){
        return {
            foo: []
        };
    },
    
    componentDidMount: function(){
        arr = [];
        $('.node').map(function(key, value) {
            arr.push(value);
        });

        this.setState({foo:arr});
    },
    
    render: function() {
        return (
            <g id='nodes'>
                {this.state.poo.map(function(svgelement, value) {
                    return <ReactNode key={value} n={svgelement}/>
                })}
                
            </g>
        );
    }
});


var ReactNode = React.createClass({
    getInitialState: function(){
        return {
            gattributes: {},
            gstyles: {}
        };
    },
    
    componentDidMount: function(){
        //reusable code
        attrs = [];
        g = this.props.n.attributes;
        Array.prototype.slice.call(g).forEach(function(item) {
            attrs[item.name] = item.value;
        });
        this.setState({gattributes: attrs});
        
        //Need to add styles as an object
        styles = {};
        //in case more than one style
        individual_styles= attrs["style"].split(";");
        individual_styles.map(function(key, value){
            if (key){
                styles[key.substring(0, key.indexOf(':'))] = key.substring(key.indexOf(':')+1);
            }
        });
        this.setState({rectstyles: styles});
    },
    
    render: function() {
        return (
            <g className='node' {... this.state.gattributes} style={this.state.gstyles} >
                <ReactRect svgelement={this.props.n.children[0]}/>
                <ReactText svgelement={this.props.n.children[1]}/>
            </g>
        );
    }
});

var ReactRect = React.createClass({
    getInitialState: function(){
        return {
            rectattributes: {},
            rectstyles: {}
        };
    },
    
    componentDidMount: function(){
        attrs = [];
        t = this.props.svgelement.attributes;
        Array.prototype.slice.call(t).forEach(function(item) {
            attrs[item.name] = item.value;
        });
        this.setState({rectattributes: attrs});
        
        //Need to add styles as an object
        styles = {};
        //in case more than one style
        individual_styles= attrs["style"].split(";");
        individual_styles.map(function(key, value){
            if (key){
                styles[key.substring(0, key.indexOf(':'))] = key.substring(key.indexOf(':')+1);
            }
        });
        this.setState({rectstyles: styles});
        
        
    },
    render: function() {
        return (
            <rect {... this.state.rectattributes} style={this.state.rectstyles}>
            </rect>
        );
    }
});

//need to do cases for two lines
var ReactText = React.createClass({
    getInitialState: function(){
        return {
            textattributes: []
        };
    },
    
    componentDidMount: function(){
        dict = [];
        t = this.props.svgelement.attributes;
        Array.prototype.slice.call(t).forEach(function(item) {
            dict[item.name] = item.value;
        });
        this.setState({textattributes: dict});
    },
    render: function() {
        return (
            <text {... this.state.textattributes}>
                {this.props.svgelement.innerHTML}
            </text>
        );
    }
});