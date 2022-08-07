import React from "react"
import PropTypes from "prop-types"
import FocusTab from "./FocusTab.js"

// These lists are in reverse order to what ends up appearing on the screen
const computerScienceFocusLabels = [
  ["ASFOC1689A", "Scientific Computing"],
  ["ASFOC1689B", "Artificial Intelligence"],
  ["ASFOC1689C", "Computational Linguistics"],
  ["ASFOC1689D", "Computer Vision"],
  ["ASFOC1689F", "Computer Systems"],
  ["ASFOC1689G", "Game Design"],
  ["ASFOC1689H", "Human\u2011Computer Interaction"],
  ["ASFOC1689I", "Theory of Computation"],
  ["ASFOC1689J", "Web Technologies"],
]

/**
 * React component representing the focus menu bar
 */
export default class FocusBar extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      open: false,
    }
  }

  /**
   * Changes whether the focus bar is open or not
   */
  toggleFocusBar = () => {
    this.setState({ open: !this.state.open })
  }

  /**
   * Creates the menu items of the focus bar using the FocusTab component
   * @returns an array of FocusTab components
   */
  generateFocusTabs = () => {
    return computerScienceFocusLabels.map(([focusId, focusTitle]) => {
      const selected = this.props.currFocus === focusId

      return (
        <FocusTab
          key={focusId}
          pId={focusId}
          focusName={focusTitle}
          selected={selected}
          highlightFocus={this.props.highlightFocus}
        />
      )
    })
  }

  render() {
    if (!this.props.focusBarEnabled) {
      return null
    } else {
      return (
        <div className="focus-menu-bar">
          <button className="focus-menu-toggle" onClick={this.toggleFocusBar}>
            {this.state.open ? "⪡ Close" : "Focuses ⪢"}
          </button>
          <div className="focuses-list">
            {this.state.open && this.generateFocusTabs()}
          </div>
        </div>
      )
    }
  }
}

FocusBar.propTypes = {
  focusBarEnabled: PropTypes.bool,
  highlightFocus: PropTypes.func,
  currFocus: PropTypes.string,
}
