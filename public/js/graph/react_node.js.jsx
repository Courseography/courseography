function foo(){
    console.log("foo");
}

var ReactSVG = React.createClass({
    render: function() {
        return (
            {/* need to see if I can pull all the svg attributes */}
            <svg width="1195" height="650">
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
