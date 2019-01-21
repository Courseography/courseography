import React from 'react';

export default function Button(props) {
    return (
        <button id={props.divId} className='graph-control-button'
            onMouseDown={props.mouseDown}
            onMouseUp={props.mouseUp}
            onMouseEnter={props.onMouseEnter}
            onMouseLeave={props.onMouseLeave}
            disabled={props.disabled}>{props.text}</button>
    );
}
