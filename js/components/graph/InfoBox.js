import React from 'react';

export default class InfoBox extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            xPos: '0',
            yPos: '0',
            nodeId: '',
            showInfobox: false
        };
    }

    render() {
        if (this.state.showInfobox) {
            //TODO: move to CSS
            var gStyles = {
                cursor: 'pointer',
                transition: 'opacity .4s',
                opacity: this.state.showInfobox ? 1 : 0
            }
            var rectAttrs = {
                id:this.state.nodeId+'-tooltip' + '-rect',
                x: this.state.xPos,
                y: this.state.yPos,
                rx: '4',
                ry: '4',
                fill: 'white',
                stroke: 'black',
                'strokeWidth': '2',
                width: '60',
                height: '30'
            };

            var textAttrs = {
                'id': this.state.nodeId +'-tooltip' + '-text',
                'x': parseFloat(this.state.xPos) + 60 / 2 - 18,
                'y': parseFloat(this.state.yPos) + 30 / 2 + 6
            };

            return (
                <g id='infoBox' className='tooltip-group' style={gStyles} {... this.props}>
                    <rect {... rectAttrs} ></rect>
                    <text {... textAttrs} >
                        Info
                    </text>
                </g>
            );
        } else {
            return <g></g>;
        }
    }
}
