function foo(){
    console.log("foo");
    React.render(
        <ReactSVG width="1195" height="650" />,
        document.getElementById('react-graph')
    );
}

var ReactSVG = React.createClass({
    render: function() {
        return (
            <svg>
                <Nodes/>
            </svg>
        );
    }
});

var Nodes = React.createClass({
    render: function() {
        return (
            <g id='nodes'>
            {/* <list of smaller components> */}
                
            </g>
        );
    }
});

