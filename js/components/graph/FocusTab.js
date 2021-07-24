import React from "react";
import PropTypes from "prop-types";
import { FocusModal } from "../common/react_modal.js.jsx";

export default class FocusTab extends React.Component {
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
    return (
      <div
        className={this.props.selected ? "focus active-focus" : "focus"}
        style={{'--background-color': this.props.color}}
      >
        <button id={this.props.pId} onClick={() => this.props.selectFocus(this.props.pId)}>
          {this.props.focusName}
        </button>
        <div className="focus-info">
          <FocusModal showFocusModal={this.state.showFocusModal} focusId={this.props.pId} onClose={() => this.toggleFocusModal(false)} />
          {this.props.selected && <button onClick={() => this.toggleFocusModal(true)}>i</button>}
        </div>
      </div>
    )
  }
}

FocusTab.propTypes = {
  focusName: PropTypes.string,
  color: PropTypes.string,
  selectFocus: PropTypes.func,
  selected: PropTypes.bool,
  pId: PropTypes.string
};
