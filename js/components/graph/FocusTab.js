import React from "react"
import PropTypes from "prop-types"
import { FocusModal } from "../common/react_modal.js.jsx"

/**
 * React component representing an item on the focus menu bar
 */
export default function FocusTab({ focusName, highlightFocus, selected, pId }) {
  const [showFocusModal, setShowFocusModal] = React.useState(false)

  /**
   * Change whether the modal popup describing this focus is shown
   * @param {bool} value
   */
  const toggleFocusModal = value => {
    setShowFocusModal(value)
  }

  return (
    <div className={selected ? "focus active-focus" : "focus"}>
      <button id={pId} onClick={() => highlightFocus(pId)}>
        {focusName}
      </button>
      <div className="focus-info">
        <FocusModal
          showFocusModal={showFocusModal}
          focusId={pId}
          onClose={() => toggleFocusModal(false)}
        />
        {selected && (
          <button onClick={() => toggleFocusModal(true)} aria-label="Focus Description">
            i
          </button>
        )}
      </div>
    </div>
  )
}

FocusTab.propTypes = {
  focusName: PropTypes.string,
  highlightFocus: PropTypes.func,
  selected: PropTypes.bool,
  pId: PropTypes.string,
}
