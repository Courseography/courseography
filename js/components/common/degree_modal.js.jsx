import React from "react"
import ReactModal from "react-modal"

function DegreeModal(props) {
  const coursesList = props.activeCourses ? [...props.activeCourses] : []

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
        </div>

        <div className="planner-selected-courses">
          Currently Selected Courses:
          {coursesList.map(course => (
            <div className="node" key={`active ${course}`}>
              <div className="rect">{course}</div>
            </div>
          ))}
        </div>
      </div>
    </ReactModal>
  )
}

export default DegreeModal
