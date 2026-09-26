import React, { useState } from "react"
import { FocusModal } from "../common/react_modal.js.jsx"

interface FocusTabProps {
  focusName: string
  highlightFocus: (pId: string) => void
  selected: boolean
  pId: string
}

/**
 * React component representing an item on the focus menu bar
 */
export default function FocusTab({
  focusName,
  highlightFocus,
  selected,
  pId,
}: FocusTabProps) {
  const [showFocusModal, setShowFocusModal] = useState(false)

  /**
   * Change whether the modal popup describing this focus is shown
   * @param value
   */
  const toggleFocusModal = (value: boolean) => {
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
