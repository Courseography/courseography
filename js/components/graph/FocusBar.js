import React from "react"
import PropTypes from "prop-types"
import FocusTab from "./FocusTab.js"

// These lists are in reverse order to what ends up appearing on the screen
const computerScienceFocusLabels = [
  ["web", "Web Technologies", "J"],
  ["theory", "Theory of Computation", "I"],
  ["sci", "Scientific Computing", "A"],
  ["HCI", "Human\u2011Computer Interaction", "H"],
  ["game", "Game Design", "G"],
  ["vision", "Computer Vision", "D"],
  ["systems", "Computer Systems", "F"],
  ["NLP", "Computational Linguistics", "C"],
  ["AI", "Artificial Intelligence", "B"],
  //  ["A", "Scientific Computing"],
  //  ["B", "Artificial Intelligence"],
  //  ["C", "Computational Linguistics"],
  //  ["D", "Computer Vision"],
  //  ["F", "Computer Systems"],
  //  ["G", "Game Design"],
  //  ["H", "Human\u2011Computer Interaction"],
  //  ["I", "Theory of Computation"],
  //  ["J", "Web Technologies"],
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
    return computerScienceFocusLabels.map(([focusId, focusTitle, focusCode]) => {
      const selected = this.props.currFocus === focusId

      return (
        <FocusTab
          key={focusId}
          pId={focusId}
          code={focusCode}
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
