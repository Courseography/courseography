import React from "react";
import PropTypes from "prop-types";
import { FocusModal } from "../common/react_modal.js.jsx";

//PRCOM: change the name of this component to FocusTab
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
    return (
      <div>
        <div
          className={this.props.selected ? "focus active-focus" : "focus"}
          style={{'--background-color': this.props.color}}>
          <button id={this.props.pId} onClick={() => this.props.selectFocus(this.props.pId)}>
            {this.props.focusName}
          </button>
        </div>
        {this.props.selected &&
          <div className="focus-info">
            <FocusModal showFocusModal={this.state.showFocusModal} focusId={this.props.pId} onClose={() => this.toggleFocusModal(false)} />
            <button onClick={() => this.toggleFocusModal(true)}>i</button>
          </div>
        }
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
