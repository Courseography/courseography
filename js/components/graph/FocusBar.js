import React from "react"
import PropTypes from "prop-types"
import FocusTab from "./FocusTab.js"

// These lists are in reverse order to what ends up appearing on the screen
const computerScienceFocusLabels = [
  ["ASFOC1689J", "Web Technologies"],
  ["ASFOC1689I", "Theory of Computation"],
  ["ASFOC1689A", "Scientific Computing"],
  ["ASFOC1689H", "Human\u2011Computer Interaction"],
  ["ASFOC1689G", "Game Design"],
  ["ASFOC1689D", "Computer Vision"],
  ["ASFOC1689F", "Computer Systems"],
  ["ASFOC1689C", "Computational Linguistics"],
  ["ASFOC1689B", "Artificial Intelligence"],
]

export default function FocusBar({focusBarEnabled, highlightFocus, currFocus}) {
  const [open, setOpen] = React.useState(false)

  /**
   * Changes whether the focus bar is open or not
   */
  const toggleFocusBar = () => {
    setOpen(!open)
  }

  /**
   * Creates the menu items of the focus bar using the FocusTab component
   * @returns an array of FocusTab components
   */
  const generateFocusTabs = () => {
    return computerScienceFocusLabels.map(([focusId, focusTitle]) => {
      const selected = currFocus === focusId

      return (
        <FocusTab
          key={focusId}
          pId={focusId}
          focusName={focusTitle}
          selected={selected}
          highlightFocus={highlightFocus}
        />
      )
    })
  }

  if (!focusBarEnabled) {
    return null
  }

  return (
    <div className="focus-menu-bar">
      <button className="focus-menu-toggle" onClick={toggleFocusBar}>
        {open ? "⪡ Close" : "Focuses ⪢"}
      </button>
      <div className="focuses-list">
        {open && generateFocusTabs()}
      </div>
    </div>
  )
}

FocusBar.propTypes = {
  focusBarEnabled: PropTypes.bool,
  highlightFocus: PropTypes.func,
  currFocus: PropTypes.string,
}
