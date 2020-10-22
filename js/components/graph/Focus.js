import React from "react";
import PropTypes from "prop-types";

export default class Focus extends React.Component {
  render() {
    const divId = this.props.pId + '-details'
    return (
      <div onClick={() => this.props.highlightFocus(this.props.pId)}>
        <p id={this.props.pId} className="focus">{this.props.focusName}</p>
        <div id={divId} className="details" />
      </div>
    )
  }
}

Focus.propTypes = {
  pId: PropTypes.string,
  focusName: PropTypes.string,
  highlightFocus: PropTypes.func
};
