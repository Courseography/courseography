import React from 'react';
import PropTypes from "prop-types";
import FocusTab from "./FocusTab.js";

// These lists are in reverse order to what ends up appearing on the screen
const computerScienceFocusLabels = [
  ["web", "Web Technologies"],
  ["theory", "Theory of Computation"],
  ["HCI", "HumanComp Interaction"],
  ["game", "Video Games"],
  ["systems", "Computer Systems"],
  ["vision", "Computer Vision"],
  ["NLP", "Computational Linguistics"],
  ["AI", "Artificial Intelligence"],
  ["sci", "Scientific Computing"],
];

/**
 * React component representing the focus menu bar
 */
export default class FocusBar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      open: false,
    };
  }

  /**
   * Changes whether the focus bar is open or not
   */
  toggleFocusBar = () => {
    if (this.state.open) {
      this.setState({open: false});
    } else {
      this.setState({open: true});
    }
}

  /**
   * Creates the menu items of the focus bar using the FocusTab component
   * @returns an array of FocusTab components
   */
  generateFocusTabs = () => {
    return computerScienceFocusLabels.map((focus) => {
      const selected = this.props.currFocus == focus[0];

      return (
        <FocusTab
          key={focus[0]}
          pId={focus[0]}
          focusName={focus[1]}
          selected={selected}
          highlightFocus={(id) => this.props.highlightFocus(id)}
        />
      )
    });
  }

  render() {
    if (!this.props.focusBarEnabled) {
      return null;
    } else {
      return (
        <div className="focus-menu-bar">
          <button className="focus-menu-toggle" onClick={this.toggleFocusBar}>{this.state.open ? "⪡ CLOSE" : "FOCUSES ⪢"}</button>
          <div className="focuses-list">
            {this.state.open && this.generateFocusTabs()}
          </div>
        </div>
      );
    }
  }
}

FocusBar.propTypes = {
  focusBarEnabled: PropTypes.bool,
  highlightFocus: PropTypes.func,
  currFocus: PropTypes.string,
};
