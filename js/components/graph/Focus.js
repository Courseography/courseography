import React from "react";
import PropTypes from "prop-types";

export default class Focus extends React.Component {
  render() {
    const divId = this.props.pId + '-details'
    return (
      <React.Fragment>
        <p id={this.props.pId} className="focus">{this.props.focusName}</p>
        <div id={divId} className="details" />
      </React.Fragment>
    )
  }
}

Focus.propTypes = {
  pId: PropTypes.string,
  focusName: PropTypes.string,
};
