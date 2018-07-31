import React from 'react';
import ReactDOM from 'react-dom';

import { CoursePanel } from './course_panel.js.jsx';
import { SearchPanel } from './search_panel.js.jsx';
import { Row } from './calendar.js.jsx';


class Grid extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedLectures: [],
      selectedCourses: []
    };

    this.addSelectedCourse = this.addSelectedCourse.bind(this);
    this.removeSelectedCourse = this.removeSelectedCourse.bind(this);
    this.clearSelectedCourses = this.clearSelectedCourses.bind(this);

    this.addSelectedLecture = this.addSelectedLecture.bind(this);
    this.removeSelectedLecture = this.removeSelectedLecture.bind(this);
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
                          !lecture.courseCode.includes(courseCode.substring(0, 6)));
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
  addSelectedLecture(newLecture) {
    // The maximum number of courses in the lecture list with the same code is 3, one for each session (F, S, Y)
    let updatedLectures = this.state.selectedLectures.filter((lecture) => {
      return lecture.courseCode !== newLecture.courseCode || lecture.session !== newLecture.session
    });
    updatedLectures.push(newLecture);
    this.setState({selectedLectures: updatedLectures});
  }

  // Method passed to child component CoursePanel to remove a lecture from selectedLectures
  removeSelectedLecture(courseCode, session) {
    let updatedLectures = this.state.selectedLectures.filter((lecture) => {
      return lecture.courseCode !== courseCode || lecture.session !== session
    });
    this.setState({selectedLectures: updatedLectures})
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
