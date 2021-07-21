import React from "react";
import PropTypes from "prop-types";

export default class Focus extends React.Component {
  render() {
    let focusClass = "focus";
    if (this.props.selected) {
      focusClass += " active-focus"
    }

    return (
      <div className={focusClass} style={{'--background-color': this.props.color}}>
        <button id={this.props.pId} onClick={() => this.props.selectFocus(this.props.pId)}>
          {this.props.focusName}
        </button>
      </div>
    )
  }
}

Focus.propTypes = {
  focusName: PropTypes.string,
  color: PropTypes.string,
  selectFocus: PropTypes.func,
  selected: PropTypes.bool,
  pId: PropTypes.string
};
