import React from 'react';
import { Modal } from '../common/react_modal.js.jsx';

/**
 * Holds courses selected from the search bar, and lists of their F, S and Y lecture, tutorial,
 * and practical sections.
 */
export class CoursePanel extends React.Component {
  constructor(props) {
    super(props);
    this.clearAllCourses = this.clearAllCourses.bind(this);
  }

  // Only clear all selected courses if the user confirms in the alert
  // pop up window.
  clearAllCourses() {
    if (window.confirm("Clear all selected courses?")) {
      this.props.clearCourses();
    }
  }

  render() {
    const courses = this.props.selectedCourses.map(
      course => <Course key={course}
                        selectedLectures={this.props.selectedLectures}
                        courseCode={course}
                        removeCourse={this.props.removeCourse}
                        addSelectedLecture={this.props.addSelectedLecture}
                        removeSelectedLecture={this.props.removeSelectedLecture}/>)

    return (
      <div id="course-select-wrapper" className="col-md-2 col-xs-6">
        <ul className="trapScroll-enabled" id="course-select">
          <li id="clear-all" key="clear-all-grid" onClick={this.clearAllCourses}>
            <h3>Clear All</h3>
          </li>
          {courses}
        </ul>
      </div>
    );
  }
}

/**
 * A selected course with a delete button, and info button, which displays information
 * about the course when clicked.
 * Also retrieves the course information. It holds list of F, S and Y lecture, tutorial
 * and practical sections for the course.
 */
class Course extends React.Component {
  constructor(props) {
    super(props);
    this.modal = null;
    this.state = {
      selected: false,
      courseInfo: {}
    }
    this.toggleSelect = this.toggleSelect.bind(this);
    this.removeCourse = this.removeCourse.bind(this);
    this.parseLectures = this.parseLectures.bind(this);
    this.displayInfo = this.displayInfo.bind(this);
    this.containsSelectedLecture = this.containsSelectedLecture.bind(this);
  }

  componentDidMount() {
    getCourse(this.props.courseCode)
      .then(data => {
        let course = {
          courseCode: "",
          F: [],
          S:[],
          Y:[]
        };
        course.courseCode = data.name;
        course.F = course.F.concat(this.parseLectures(data.fallSession.lectures),
                                    this.parseLectures(data.fallSession.tutorials),
                                    this.parseLectures(data.fallSession.practicals));
        course.S = course.S.concat(this.parseLectures(data.springSession.lectures),
                                    this.parseLectures(data.springSession.tutorials),
                                    this.parseLectures(data.springSession.practicals));
        course.Y = course.Y.concat(this.parseLectures(data.yearSession.lectures),
                                    this.parseLectures(data.yearSession.tutorials),
                                    this.parseLectures(data.yearSession.practicals));
        this.setState({courseInfo: course});
    });
  }

  parseLectures(lectures) {
    // Remove duplicated lecture sections
    const allLectures = removeDuplicateLectures(lectures);
    let parsedLectures = [];

    // Loop through the lecture sections to get each section's session code and lecture times
    allLectures.forEach( lectureInfo => {
      // Check to make sure its not an online section (online sections have course codes beginning with 9) or
      // restricted section. Restricted sections have enrollment restricted for a particular group of students,
      // but happens at the same time and place as a regular lecture/tutorial section.
      if (lectureInfo.section.charAt(3) !== '2' && lectureInfo.section.charAt(3) !== '9' &&
        lectureInfo.times !== 'Online Web Version') {
        let lecture = {
          courseCode: lectureInfo.code.substring(0, 6) + " (" + lectureInfo.section.substring(0,1) + ")",
          lectureCode: lectureInfo.section.substring(0, 1) + lectureInfo.section.substring(3),
          session: lectureInfo.session,
          times: lectureInfo.times,
        };
        parsedLectures.push(lecture);
      }
    });
    return parsedLectures;
  }

  toggleSelect() {
    this.setState({selected: !this.state.selected})
  }

  removeCourse() {
    this.props.removeCourse(this.props.courseCode);
  }

  displayInfo() {
    this.modal.openModal(this.state.courseInfo.courseCode.substring(0, 6));
  }

  containsSelectedLecture() {
    // Only use method subString on the value of this.state.courseInfo.courseCode if
    // if the this.state.courseInfo.courseCode exists (ie the course information has already been fetched)
    if (this.state.courseInfo.courseCode) {
      const lectures = this.props.selectedLectures.map(lecture => lecture.courseCode.substring(0, 6));
      const courseCode = (this.state.courseInfo.courseCode)
      return lectures.indexOf(courseCode.substring(0, 6)) >= 0;
    }
    return false;
  }

  render() {
    return (
      <li key={this.props.courseCode} id={this.props.courseCode + "-li"} className={"ui-accordion ui-widget ui-helper-reset"}>
        <div className="ui-accordion-header ui-helper-reset ui-state-default ui-accordion-icons ui-accordion-header-active ui-state-active ui-corner-top"
              id={"ui-accordion-" + this.props.courseCode + "-li-header-0"}>
          <Modal ref={ r => this.modal = r}/>
          <div className="icon-div">
            <img src="static/res/ico/delete.png" className="close-icon" onClick={this.removeCourse}/>
            <img src="static/res/ico/about.png" className="close-icon" onClick={this.displayInfo}/>
          </div>
          <h3 onClick={this.toggleSelect} data-satisfied="true" taken={this.containsSelectedLecture() ? "true" : "false"}>
            {this.props.courseCode}
          </h3>
        </div>
        { this.state.selected &&
          <div className="sections ui-accordion-content ui-helper-reset ui-widget-content ui-corner-bottom ui-accordion-content-active"
                id={"ui-accordion-" + this.props.courseCode + "-li-panel-0"}>
            <SectionList courseCode={this.props.courseCode}
                          session="Y"
                          lectures={this.state.courseInfo.Y}
                          addSelectedLecture={this.props.addSelectedLecture}
                          selectedLectures={this.props.selectedLectures}
                          removeSelectedLecture={this.props.removeSelectedLecture}/>
            <SectionList courseCode={this.props.courseCode}
                          session="F"
                          lectures={this.state.courseInfo.F}
                          addSelectedLecture={this.props.addSelectedLecture}
                          selectedLectures={this.props.selectedLectures}
                          removeSelectedLecture={this.props.removeSelectedLecture}/>
            <SectionList courseCode={this.props.courseCode}
                          session="S"
                          lectures={this.state.courseInfo.S}
                          addSelectedLecture={this.props.addSelectedLecture}
                          selectedLectures={this.props.selectedLectures}
                          removeSelectedLecture={this.props.removeSelectedLecture}/>
          </div>
        }
      </li>
    )
  }
}

/**
 * A list of lecture, tutorial and practical sections for the specified course for the specified
 * session.
 */
class SectionList extends React.Component {
  render() {
    const lectureSections = this.props.lectures.map(lecture =>
      <LectureSection key={this.props.courseCode + lecture.lectureCode + this.props.section}
                      session={this.props.session}
                      courseCode={this.props.courseCode}
                      lecture={lecture}
                      addSelectedLecture={this.props.addSelectedLecture}
                      selectedLectures={this.props.selectedLectures}
                      removeSelectedLecture={this.props.removeSelectedLecture}/>);
    return(
      <ul className={"sectionList-" + this.props.session} id="lecture-list">
        {lectureSections}
      </ul>
    )
  }
}

/**
 * A lecture, tutorial or practical section for the specified course in the specified
 * session. The section is added to or removed from a list of selected lecture upon click.
 */
class LectureSection extends React.Component {
  constructor(props) {
    super(props);
    this.selectLecture = this.selectLecture.bind(this);
    this.isSelectedLecture = this.isSelectedLecture.bind(this);
  }

  // Remove the lecture if it is already in the selectedLectures list, or add the lecture if it is not.
  selectLecture() {
    if (this.isSelectedLecture()) {
      this.props.removeSelectedLecture(this.props.lecture.courseCode, this.props.session);
    } else {
      this.props.addSelectedLecture(this.props.lecture);
    }
  }

  // Check whether the lecture is in the selectedLectures list, return true if it is, false it is not.
  isSelectedLecture() {
    const sameLecture = this.props.selectedLectures.filter((lecture) => {
      return lecture.courseCode === this.props.lecture.courseCode && lecture.session === this.props.session &&
        lecture.lectureCode === this.props.lecture.lectureCode
    });
    // If sameLecture is not an empty array, then this lecture is already selected and should be removed
    return sameLecture.length > 0;
  }

  render() {
    return(
      <li id={this.props.courseCode + "-" + this.props.lecture.lectureCode + "-" + this.props.session}
          onClick={this.selectLecture}
          clicked={this.isSelectedLecture() ? "true" : "false"}
          data-satisfied={"true"}>
        {this.props.lecture.lectureCode}
      </li>
    )
  }
}
