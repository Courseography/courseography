import React from "react";
import PropTypes from "prop-types";

export default function Button(props) {
  return (
    <button
      id={props.divId}
      className="graph-control-button"
      onMouseDown={props.mouseDown}
      onMouseUp={props.mouseUp}
      onMouseEnter={props.onMouseEnter}
      onMouseLeave={props.onMouseLeave}
      disabled={props.disabled}
    >
      {props.text}
      {props.children}
    </button>
  );
}

Button.propTypes = {
  divId: PropTypes.string,
  mouseDown: PropTypes.func,
  mouseUp: PropTypes.func,
  onMouseEnter: PropTypes.func,
  onMouseLeave: PropTypes.func,
  disabled: PropTypes.bool,
  text: PropTypes.string,
  children: PropTypes.node,
};
