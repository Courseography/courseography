import React from 'react';
import ReactDOM from 'react-dom';

import { CoursePanel } from './course_panel.js.jsx';
import { SearchPanel } from './search_panel.js.jsx';
import { Row } from './calendar.js.jsx';


class Grid extends React.Component {
  constructor(props) {
    super(props);
    this.addSelectedCourse = this.addSelectedCourse.bind(this);
    this.removeSelectedCourse = this.removeSelectedCourse.bind(this);
    this.clearSelectedCourses = this.clearSelectedCourses.bind(this);

    this.addSelectedLecture = this.addSelectedLecture.bind(this);
    this.removeSelectedLecture = this.removeSelectedLecture.bind(this);
    this.createNewCourse = this.createNewCourse.bind(this);
    this.createNewLecture = this.createNewLecture.bind(this);
    this.state = {
      selectedLectures: [],
      selectedCourses: []
    };
  }

  // get the previously selected courses and lecture sections from local storage
  componentDidMount() {
    let selectedCoursesLocalStorage = localStorage.getItem('selectedCourses');
    let selectedLecturesLocalStorage = localStorage.getItem('selectedLectures');

    if (!selectedLecturesLocalStorage) {
      selectedLecturesLocalStorage = [];
    } else {
      try {
        this.setState({selectedLectures: JSON.parse(selectedLecturesLocalStorage)});
      }
      catch (e) {
        console.log(e);
      }
    }

    if (!selectedCoursesLocalStorage) {
      selectedCoursesLocalStorage = [];
    } else {
      selectedCoursesLocalStorage = selectedCoursesLocalStorage.split('_');
      let selectedCourses = [];
      selectedCoursesLocalStorage.forEach((courseCode) => {
        // Not using this.addSelectedCourse(courseCode) because each time addSelectedCourse is
        // called, this.setState is used.
        // setState is asynchronous and calling it several times in a row can lead to bugs when
        // new state depends on previous state
        selectedCourses.push(courseCode);
      });
      this.setState({selectedCourses: selectedCourses});
    }
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.selectedCourses !== prevState.selectedCourses) {
      localStorage.setItem("selectedCourses", this.state.selectedCourses.join('_'));
    }
    if (this.state.selectedLectures !== prevState.selectedLectures) {
      localStorage.setItem("selectedLectures", JSON.stringify(this.state.selectedLectures));
    }
  }

  // Method passed to child component SearchPanel to add a course to selectedCourses.
  addSelectedCourse(courseCode) {
    // updatedCourses is a copy of this.state.selectedCourses so that prevState can be distinguished from
    // the current state in lifecycle methods like componentDidUpdate
    let updatedCourses = this.state.selectedCourses.slice();
    updatedCourses.push(courseCode);
    this.setState({selectedCourses: updatedCourses});
  }

  // Method passed to child components, SearchPanel and CoursePanel to remove a course from selectedCourses.
  removeSelectedCourse(courseCode) {
    let updatedCourses = this.state.selectedCourses.slice();
    const index = updatedCourses.indexOf(courseCode);
    updatedCourses.splice(index, 1);
    this.setState({selectedCourses: updatedCourses});

    let updatedLectures = this.state.selectedLectures.filter(lecture =>
                          !lecture.course.includes(courseCode.substring(0, 6)));
    this.setState({selectedLectures: updatedLectures});
  }

  // Method passed to child component CoursePanel to clear all the courses in selectedCourses.
  clearSelectedCourses() {
    this.setState({
      selectedCourses: [],
      selectedLectures: []
    });
  }

  // Method passed to child component CoursePanel to add a lecture to selectedLectures
  addSelectedLecture(courseCode, session, lectureCode, lectureTimes) {
    // The maximum number of courses in the lecture list with the same code is 3, one for each session (F, S, Y)
    let updatedLectures = this.state.selectedLectures.filter((lecture) => {
      return lecture.course !== courseCode || lecture.session !== session
    });
    let lectureSession = this.createNewCourse(courseCode, session, lectureCode, lectureTimes);
    updatedLectures.push(lectureSession);
    this.setState({selectedLectures: updatedLectures});
  }

  // Method passed to child component CoursePanel to remove a lecture from selectedLectures
  removeSelectedLecture(courseCode, session) {
    let updatedLectures = this.state.selectedLectures.filter((lecture) => {
      return lecture.course !== courseCode || lecture.session !== session
    });
    this.setState({selectedLectures: updatedLectures})
  }

  /**
   * Constructor for a 'Course' object
   * @param {string} courseCode : Name of course
   * @param {string} session : Session of course
   * @param {dictionary} times : The days, and corresponding time-slot for which
                                  this course is active
   * @return {dictionary} Represents a 'Course' object
  */
  createNewCourse(courseCode, session, lectureCode, times) {
    let lectures = {};
    let days = [];
    // Store the active days of this Course, and for each active
    // day, create and store a 'Lecture' object
    for (let day in times) {
      days.push(day);
      lectures[day] = [];
      // For the case where this lecture starts and ends more than once in one day
      for(let i = 0; i < times[day].length; i+=2){
        let startEndTimes = [times[day][i], times[day][i+1]];
        lectures[day].push(this.createNewLecture(courseCode, session, day, startEndTimes));
      }
    }
    let courseObject = {};
    courseObject.course = courseCode;
    courseObject.lectureCode = lectureCode;
    courseObject.session = session;
    courseObject.days = days;
    courseObject.lectures = lectures;
    return courseObject;
  }

  /**
   * Constructor for a 'Lecture' object. Represents a single period of time
   * in which this course is active. (for ex; on Monday from 2-4)
   * @param {string} courseCode : Name of Lecture
   * @param {string} session : Session of Lecture
   * @param {string} day: Day of activity
   * @param {list} timePeriod : Active start and end time
   * @return {dictionary} Represents a 'Lecture' object
  */
  createNewLecture(courseCode, session, day, timePeriod) {
    let lectureObject = {};
    lectureObject.courseCode = courseCode;
    lectureObject.session = session;
    lectureObject.day = day;
    lectureObject.startTime = timePeriod[0];
    lectureObject.endTime = timePeriod[1];
    lectureObject.inConflict = false;
    // The width of a lecture is the maximum number of conflicts it has while it's active;
    //  if there are no conflicts, the width is 1.
    lectureObject.width = 1;
    return lectureObject;
  }

  render() {
    return (
      <div>
        <CoursePanel
          selectedCourses={this.state.selectedCourses}
          selectedLectures={this.state.selectedLectures}
          removeCourse={this.removeSelectedCourse}
          clearCourses={this.clearSelectedCourses}
          addSelectedLecture={this.addSelectedLecture}
          removeSelectedLecture={this.removeSelectedLecture}
        />
        <Row courses={this.state.selectedLectures}/>
        <SearchPanel
          selectedCourses={this.state.selectedCourses}
          selectCourse={this.addSelectedCourse}
          removeCourse={this.removeSelectedCourse}
        />
      </div>
    );
  }
}


ReactDOM.render(
  <Grid />,
  document.getElementById('grid-body')
);
