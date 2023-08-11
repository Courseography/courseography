import React from "react"
import ReactModal from "react-modal"
import { DragDropContext } from "react-beautiful-dnd"

function DegreeModal(props) {
  const coursesList = props.activeCourses ? [...props.activeCourses] : []

  return (
    <ReactModal
      className="degree-planner-modal"
      overlayClassName="overlay"
      isOpen={props.showDegreeModal}
      onRequestClose={props.onClose}
      ariaHideApp={false}
    >
      <div className="degree-planner-header">
        <div>Courseography Degree Planner</div>
      </div>

      <div className="selected-courses-container">
        Currently Selected Courses:
        <div className="selected-courses">
          {coursesList.map(course => (
            <div key={`active ${course}`}>
              <div className="course-node">{course}</div>
            </div>
          ))}
        </div>
      </div>

      <div className="degree-planner-calendar">
        <div className="school-year">
          <div className="year"> 2023-2024 </div>
          <div className="semester-container">
            <div className="semester-name"> Fall </div>
            <div className="semester-name"> Winter </div>
            <div className="semester-name"> Summer </div>
          </div>
          <div className="semester-courses-container">
            <div className="semester-courses"> </div>
            <div className="semester-courses"> </div>
            <div className="semester-courses"> </div>
          </div>
        </div>
        <div className="school-year">
          <div className="year"> 2024-2025 </div>
          <div className="semester-container">
            <div className="semester-name"> Fall </div>
            <div className="semester-name"> Winter </div>
            <div className="semester-name"> Summer </div>
          </div>
          <div className="semester-courses-container">
            <div className="semester-courses"> </div>
            <div className="semester-courses"> </div>
            <div className="semester-courses"> </div>
          </div>
        </div>
      </div>
    </ReactModal>
  )
}

export default DegreeModal
