import React from 'react';
import { Modal } from '../common/react_modal.js.jsx';

/**
 * Creates a search box and stores the current user input that is in the search box
 * Holds courses selected from the search box, and lists of their F, S and Y lecture, tutorial,
 * and practical sections.
 */
export class CoursePanel extends React.Component {
  constructor(props) {
    super(props);
    this.state = { value: '' };
    this.handleInput = this.handleInput.bind(this);
    this.clearAllCourses = this.clearAllCourses.bind(this);
    this.selectCourse = this.props.selectCourse.bind(this);
  }

  handleInput(event) {
    this.setState({ value: event.target.value });
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
                        hoverLecture={this.props.hoverLecture}
                        unhoverLecture={this.props.unhoverLecture}
                        selectLecture={this.props.selectLecture}/>)

    return (
      <div id="search-layout" className="col-md-3 col-xs-6">
        <div id="filter-container">
          <form onSubmit={() => false}>
            <input
              id="course-filter"
              className="form-control"
              placeholder="Enter a course!"
              autoComplete="off"
              type="text"
              value={this.state.value}
              onChange={this.handleInput}
            />
          </form>
        </div>
        <div id="search-container">
          <CourseList
            courseFilter={this.state.value.toUpperCase()}
            selectedCourses={this.props.selectedCourses}
            selectCourse={this.props.selectCourse}
            removeCourse={this.props.removeCourse}
          />
        </div>
        <div id="course-select-wrapper">
          <ul className="trapScroll-enabled" id="course-select">
            <li id="clear-all" key="clear-all-grid" onClick={this.clearAllCourses}>
              <h3>Clear All</h3>
            </li>
            {courses}
          </ul>
        </div>
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
          S: [],
          Y: []
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
      if (lectureInfo.meetingData.section.charAt(3) !== '2' && lectureInfo.meetingData.section.charAt(3) !== '9' &&
        lectureInfo.meetingData.times !== 'Online Web Version') {
        let lecture = {
          courseCode: lectureInfo.meetingData.code + " (" + lectureInfo.meetingData.section.substring(0,1) + ")",
          lectureCode: lectureInfo.meetingData.section.substring(0, 1) + lectureInfo.meetingData.section.substring(3),
          session: lectureInfo.meetingData.session,
          times: lectureInfo.timesData,
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
                          hoverLecture={this.props.hoverLecture}
                          unhoverLecture={this.props.unhoverLecture}
                          selectLecture={this.props.selectLecture}/>
            <SectionList courseCode={this.props.courseCode}
                          session="F"
                          lectures={this.state.courseInfo.F}
                          hoverLecture={this.props.hoverLecture}
                          unhoverLecture={this.props.unhoverLecture}
                          selectLecture={this.props.selectLecture}/>
            <SectionList courseCode={this.props.courseCode}
                          session="S"
                          lectures={this.state.courseInfo.S}
                          hoverLecture={this.props.hoverLecture}
                          unhoverLecture={this.props.unhoverLecture}
                          selectLecture={this.props.selectLecture}/>
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
                      hoverLecture={this.props.hoverLecture}
                      unhoverLecture={this.props.unhoverLecture}
                      selectLecture={this.props.selectLecture}/>);
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
const LectureSection = (props) => {
  return (
    <li id={props.courseCode + "-" + props.lecture.lectureCode + "-" + props.session}
        onClick={ () => props.selectLecture(props.lecture) }
        onMouseOver={ () => props.hoverLecture(props.lecture) }
        onMouseOut={ () => props.unhoverLecture() }
        data-satisfied={"true"}>
      {props.lecture.lectureCode}
    </li>
  )
}

class CourseList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      courses: [],
    };
  }

  componentDidMount() {
    // AJAX requests allow the programmer to:
    //    1. update a webpage without refreshing
    //    2. Request data from a server AFTER the webpage is loaded
    //    3. Send data to the server - in the background

    // This makes an AJAX call to retrieve courses from the database
    fetch(
      'all-courses', // url to which the AJAX request is sent to
    )
      .then(response => response.text())
      .then(data => {
        // searches through all of the courses in "data",
        // and stores each individual course code name
        // into 'courses' list
        let courses = data.split('\n').map(course => course.substring(0, 8));
        this.setState({ courses: courses });
      });
  }

  render() {
    let searchList = [];
    // If there are courses to be filtered
    if (this.props.courseFilter !== '') {
      // From the "courses" list, filter out elements based off of the prop "courseFilter" passed to
      // CourseList by SearchPanel
      searchList = this.state.courses.filter(
        course => course.indexOf(this.props.courseFilter) > -1
      ).map(course => <CourseEntry
          course={course}
          key={course}
          selectCourse={this.props.selectCourse}
          removeCourse={this.props.removeCourse}
          selectedCourses={this.props.selectedCourses}
        />
      );
    }

    // Return all the unfiltered courses in the "courses" list in a list
    return (
      <div id="search-list">
        <ul>{searchList}</ul>
      </div>
    );
  }
}

/**
 * Describes a course based on its course code, and whether or not it has been selected
 * (If the course is selected, it is a "starred-course").
 */
class CourseEntry extends React.Component {
  constructor(props) {
    super(props);
    this.select = this.select.bind(this);
  }

  // Check whether the course is already in the selectCourses list.
  // Remove the course if it is, or add the course if it is not.
  select() {
    if (this.props.selectedCourses.indexOf(this.props.course) != -1) {
      this.props.removeCourse(this.props.course);
    } else {
      this.props.selectCourse(this.props.course);
    }
  }

  render() {
    const classes = this.props.selectedCourses.indexOf(this.props.course) != -1 ? 'starred-course' : '';
    return (
      <li id={this.props.course + '-search'}
          className={classes}
          onClick={this.select}>
        {this.props.course}
      </li>
    );
  }
}
