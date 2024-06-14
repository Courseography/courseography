import React from "react"
import PropTypes from "prop-types"

export default function ErrorMessage({ message, onClose }) {
  return (
    <div className="error-overlay">
      <div className="error-message-box">
        <p>{message}</p>
        <button onClick={onClose} className="error-close-button">
          Close
        </button>
      </div>
    </div>
  )
}

ErrorMessage.propTypes = {
  message: PropTypes.string,
  onClose: PropTypes.func,
}
