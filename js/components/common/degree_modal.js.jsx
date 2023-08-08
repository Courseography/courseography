import React from "react"
import ReactModal from "react-modal"

function DegreeModal(props) {
  return (
    <ReactModal
      isOpen={props.showDegreeModal}
      className="modal-class"
      overlayClassName="overlay"
      onRequestClose={props.onClose}
      ariaHideApp={false}
    >
      <div>Hello</div>
    </ReactModal>
  )
}

export default DegreeModal
