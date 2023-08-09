import React from "react"
import ReactModal from "react-modal"

function DegreeModal(props) {
  return (
    <ReactModal
      className="modal-class"
      overlayClassName="overlay"
      isOpen={props.showDegreeModal}
      onRequestClose={props.onClose}
      ariaHideApp={false}
    >
      <div>Hello</div>
    </ReactModal>
  )
}

export default DegreeModal
