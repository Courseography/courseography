import React from "react"
import PropTypes from "prop-types"
import ReactModal from "react-modal"

export default function ErrorMessage({ message, onClose }) {
  return (
    <ReactModal
      className="error-modal-class"
      overlayClassName="error-overlay"
      isOpen={true}
      ariaHideApp={false}
    >
      <div className="modal-header">
        Invalid Course Input
        <div className="button-container">
          <button onClick={onClose} className="error-close-button" type="button">
            Close
          </button>
        </div>
      </div>

      <div className="modal-body">{message}</div>
    </ReactModal>
  )
}

ErrorMessage.propTypes = {
  message: PropTypes.string,
  onClose: PropTypes.func,
}
