import React from "react"
import ReactModal from "react-modal"

function DegreeModal(props) {
  return (
    <ReactModal
      className="degree-planner-modal-class"
      overlayClassName="overlay"
      isOpen={props.showDegreeModal}
      onRequestClose={props.onClose}
      ariaHideApp={false}
    >
      <div className="degree-planner">
        <div className="planner-header">
          <div>Courseography Degree Planner</div>
          <div>X</div>
        </div>

        <div className="planner-selected-courses">Currently Selected Courses:</div>
      </div>
    </ReactModal>
  )
}

export default DegreeModal
