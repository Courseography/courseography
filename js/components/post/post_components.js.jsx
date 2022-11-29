import React from "react"
import { CourseCategory2 } from "./course_components.js.jsx"
import { CourseModal } from "../common/react_modal.js.jsx"
import * as Data from "./post_data"

/**
 * Returns whether course is a specialist course or not
 * @param {string} course Name of course
 * @return {boolean} True if course is a specialist, False otherwise
 */
function notSpecialistCourse(course) {
  return Data.specialistCourses.indexOf(course) === -1
}

function isInquiryCourse(course) {
  return Data.CSCinq.indexOf(course) >= 0
}

function isLevel400(course, level400Array) {
  return (
    notSpecialistCourse(course) &&
    course.substring(3, 4) === "4" &&
    level400Array.length < 3
  )
}

function isLevel300(course, level300Array) {
  return (
    notSpecialistCourse(course) &&
    course.substring(3, 4) >= "3" &&
    level300Array.length < 3
  )
}

function isLevelExtra(course, levelExtraArray) {
  return (
    notSpecialistCourse(course) &&
    course.substring(3, 4) >= "3" &&
    levelExtraArray.length < 4
  )
}

function updateActiveCourses() {
  return Data.allCourses.concat(Data.math).filter(course => {
    const status = localStorage.getItem(course.toLowerCase())
    return status === "active" || status === "overridden"
  })
}

class Post extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      selected: false,
      activeCourses: updateActiveCourses(),
      creditCount: 0,
    }

    this.getCourses = this.getCourses.bind(this)
    this.getInquiryCourse = this.getInquiryCourse.bind(this)
    this.changeCreditCount = this.changeCreditCount.bind(this)
    this.calculateCreditCount = this.calculateCreditCount.bind(this)
    this.openModal = this.openModal.bind(this)
  }

  componentWillMount() {
    this.setState({
      selected: localStorage.getItem(this.props.postType) === "active",
    })
    this.calculateCreditCount()
  }

  componentWillReceiveProps(newProps) {
    this.setState({ selected: newProps.isSelected })
  }

  getCourses() {
    const courseChecks = this.props.courseChecks
    const courseArrays = []

    // initialize inner arrays
    for (let i = 0; i < courseChecks.length; i++) {
      courseArrays.push([])
    }

    this.state.activeCourses.forEach(function (course) {
      for (let i = 0; i < courseChecks.length; i++) {
        if (courseChecks[i](course, courseArrays[i])) {
          courseArrays[i].push(course)
          break
        }
      }
    })

    return courseArrays
  }

  getInquiryCourse() {
    const inquiryCourses = this.state.activeCourses.filter(isInquiryCourse)
    return inquiryCourses.length === 0 ? "" : inquiryCourses[0]
  }

  changeCreditCount(value) {
    this.setState({ creditCount: this.state.creditCount + value })
  }

  calculateCreditCount() {
    let count = 0

    this.state.activeCourses.forEach(course => {
      const courseID = course.toLowerCase()
      if (
        localStorage.getItem(courseID) === "active" ||
        localStorage.getItem(courseID) === "overridden"
      ) {
        if (course === "MAT135136137157Calc1") {
          count += 1
        } else {
          count += 0.5
        }
      }
    })

    this.changeCreditCount(count)
  }

  openModal(nodeId) {
    const modal = this.refs.modal
    const newCourse = nodeId.substring(0, 6)
    modal.openModal(newCourse)
  }

  render() {
    if (this.state.selected) {
      var classes = "post_selected"
    } else {
      var classes = "post_not_selected"
    }

    const courseCategoryArrays = this.getCourses()

    return (
      <div id={"post_" + this.props.postType} className={classes}>
        <CourseModal ref="modal" />

        <CourseCategory2
          yearName="First Year"
          courses={this.props.firstYearCourses}
          openModal={this.openModal}
          titles={[]}
          otherInfo={this.props}
          courseCategoryArrays={courseCategoryArrays}
          changeCreditCount={this.changeCreditCount}
          getInquiryCourse={this.getInquiryCourse}
        />

        <CourseCategory2
          yearName="Second Year"
          courses={this.props.secondYearCourses}
          openModal={this.openModal}
          titles={[]}
          otherInfo={this.props}
          courseCategoryArrays={courseCategoryArrays}
          changeCreditCount={this.changeCreditCount}
          getInquiryCourse={this.getInquiryCourse}
        />

        <CourseCategory2
          yearName="Later Years"
          courses={this.props.laterYearCourses}
          openModal={this.openModal}
          otherInfo={this.props}
          titles={this.props.categoryTitles}
          courseCategoryArrays={courseCategoryArrays}
          changeCreditCount={this.changeCreditCount}
          getInquiryCourse={this.getInquiryCourse}
        />

        <div id="notes">
          <h3>Notes</h3>
          <ul>
            {this.props.notes.map((note, i) => (
              <li key={i}>{note}</li>
            ))}
          </ul>
        </div>
      </div>
    )
  }
}

export class SpecialistPost extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      selected: false,
      completed: false,
    }
    this.changeTabView = this.changeTabView.bind(this)
    this.setIfCompleted = this.setIfCompleted.bind(this)
    this.getCreditCount = this.getCreditCount.bind(this)
  }

  changeTabView(isSelected) {
    this.setState({ selected: isSelected })
  }

  setIfCompleted() {
    const isCompleted = this.refs.post.state.creditCount >= 12.0
    this.setState({ completed: isCompleted })
    return isCompleted
  }

  getCreditCount() {
    return this.refs.post.state.creditCount < 12.0
      ? this.refs.post.state.creditCount
      : 12.0
  }

  render() {
    const categoryTitles = [
      "Any 400-level CSC course, BCB410H, BCB420H, BCB430Y, ECE489H (1.5 FCEs)",
      "Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.5 FCEs)",
      "Any of the following: 300+ level CSC course; MAT: 235/237/257, any 300+ \
                               except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H; \
                               BCB: 410H/420H/430Y (2.0 FCEs)",
    ]
    const notes = [
      "No more than 1.0 FCE from CSC490H1, CSC491H1, CSC494H1, CSC495H1, BCB430Y1 may be used \
                     to fulfill program requirements",
    ]

    const firstYearCourses = [
      ["csc108"],
      ["csc148"],
      ["csc165", "csc240"],
      ["mat135", "mat136", "mat137", "mat157"],
    ]
    const secondYearCourses = [
      ["csc207"],
      ["csc209"],
      ["csc236", "csc240"],
      ["csc258"],
      ["csc263", "csc265"],
      ["mat221", "mat223", "mat240"],
      ["sta247", "sta255", "sta257"],
    ]
    const laterYearCourses = [["csc369"], ["csc373"]]

    return (
      <Post
        postType="specialist"
        ref="post"
        firstYearCourses={firstYearCourses}
        secondYearCourses={secondYearCourses}
        laterYearCourses={laterYearCourses}
        textBoxes={[
          [3, true],
          [3, true],
          [4, false],
        ]}
        courseChecks={[isLevel400, isLevel300, isLevelExtra]}
        categoryTitles={categoryTitles}
        notes={notes}
        hasInquiryCategory={true}
        isSelected={this.state.selected}
      />
    )
  }
}

export class MajorPost extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      selected: false,
      completed: false,
    }
    this.changeTabView = this.changeTabView.bind(this)
    this.setIfCompleted = this.setIfCompleted.bind(this)
    this.getCreditCount = this.getCreditCount.bind(this)
  }

  changeTabView(isSelected) {
    this.setState({ selected: isSelected })
  }

  setIfCompleted() {
    const isCompleted = this.refs.post.state.creditCount >= 8.0
    this.setState({ completed: isCompleted })
    return isCompleted
  }

  getCreditCount() {
    return this.refs.post.state.creditCount < 8.0
      ? this.refs.post.state.creditCount
      : 8.0
  }

  isLevel400(course, level400Array) {
    return course.substring(3, 4) === "4" && level400Array.length < 1
  }

  isLevel300(course, level300Array) {
    return course.substring(3, 4) >= "3" && level300Array.length < 2
  }

  isLevelExtra(course, levelExtraArray) {
    return course.substring(3, 4) >= "3" && levelExtraArray.length < 3
  }

  render() {
    const categoryTitles = [
      "Any 400-level CSC course, BCB410H, BCB420H, BCB430Y (0.5 FCEs)",
      "Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.0 FCEs)",
      "Any of the following: 200+ level CSC course; MAT: 221/223/240, 235/237/257, any 300+ \
                               except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H; BCB: 410H/420H/430Y \
                              (1.5 FCEs, with at least 0.5 FCEs in the 300+ level)",
    ]
    const notes = [
      "No more than 1.0 FCE from CSC490H1, CSC491H1, CSC494H1, CSC495H1, BCB430Y1 may be used \
                     to fulfill program requirements",
    ]

    const firstYearCourses = [
      ["csc108"],
      ["csc148"],
      ["csc165", "csc240"],
      ["mat135", "mat136", "mat137", "mat157"],
    ]
    const secondYearCourses = [
      ["csc207"],
      ["csc236", "csc240"],
      ["csc258"],
      ["csc263", "csc265"],
      ["sta247", "sta255", "sta257"],
    ]
    const laterYearCourses = []

    return (
      <Post
        postType="major"
        ref="post"
        firstYearCourses={firstYearCourses}
        secondYearCourses={secondYearCourses}
        laterYearCourses={laterYearCourses}
        textBoxes={[
          [1, true],
          [2, true],
          [3, false],
        ]}
        courseChecks={[this.isLevel400, this.isLevel300, this.isLevelExtra]}
        categoryTitles={categoryTitles}
        notes={notes}
        hasInquiryCategory={true}
        isSelected={this.state.selected}
      />
    )
  }
}

export class MinorPost extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      selected: false,
      completed: false,
    }
    this.changeTabView = this.changeTabView.bind(this)
    this.setIfCompleted = this.setIfCompleted.bind(this)
    this.getCreditCount = this.getCreditCount.bind(this)
  }

  changeTabView(isSelected) {
    this.setState({ selected: isSelected })
  }

  setIfCompleted() {
    const isCompleted = this.refs.post.state.creditCount >= 4.0
    this.setState({ completed: isCompleted })
    return isCompleted
  }

  getCreditCount() {
    return this.refs.post.state.creditCount < 4.0
      ? this.refs.post.state.creditCount
      : 4.0
  }

  isLevelExtra(course, levelExtraArray) {
    const nonValidCourses = ["CSC207", "CSC236240"]
    return (
      course.substring(3, 4) >= "2" &&
      nonValidCourses.indexOf(course) < 0 &&
      levelExtraArray.length < 3
    )
  }

  render() {
    const categoryTitles = [
      "200+ CSC courses (1.5 FCEs, with at least 1.0 FCE in the 300+ levels)",
    ]
    const notes = ["You may take no more than three 300+ CSC/ECE courses"]

    const firstYearCourses = [["csc108"], ["csc148"], ["csc165", "csc240"]]
    const secondYearCourses = [["csc207"], ["csc236", "csc240"]]
    const laterYearCourses = []

    return (
      <Post
        postType="minor"
        ref="post"
        firstYearCourses={firstYearCourses}
        secondYearCourses={secondYearCourses}
        laterYearCourses={laterYearCourses}
        textBoxes={[[3, false]]}
        courseChecks={[this.isLevelExtra]}
        categoryTitles={categoryTitles}
        notes={notes}
        hasInquiryCategory={false}
        isSelected={this.state.selected}
      />
    )
  }
}
