import React from "react";
import PropTypes from "prop-types";
import { FocusModal } from "../common/react_modal.js";

export default class Focus extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      showFocusModal: false,
    };
  }

  toggleFocusModal = value => {
    this.setState({
      showFocusModal: value
    });
  }

  render() {
    let focusClass = "focus";
    let focusInfoDiv;
    if (this.props.selected) {
      focusClass += " active-focus";
      focusInfoDiv = (
        <div className="focus-info">
          <FocusModal showFocusModal={this.state.showFocusModal} focusId={this.props.pId} onClose={() => this.toggleFocusModal(false)} />
          <button onClick={() => this.toggleFocusModal(true)}>i</button>
        </div>
      );
    }

    return (
      <div>
        <div className={focusClass} style={{'--background-color': this.props.color}}>
          <button id={this.props.pId} onClick={() => this.props.selectFocus(this.props.pId)}>
            {this.props.focusName}
          </button>
        </div>
        {focusInfoDiv}
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
