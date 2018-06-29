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

  // Method passed to child component SearchPanel to add a course to selectedCourses.
  // Move into child didMount
  addSelectedCourse(courseCode) {
    let updatedCourses = this.state.selectedCourses;
    updatedCourses.push(courseCode);
    this.setState({selectedCourses: updatedCourses});
  }

  // Method passed to child components, SearchPanel and CoursePanel to remove a course from selectedCourses.
  removeSelectedCourse(courseCode) {
    let updatedCourses = this.state.selectedCourses;
    const index = updatedCourses.indexOf(courseCode);
    updatedCourses.splice(index, 1);
    this.setState({selectedCourses: updatedCourses});

    let updatedLectures = this.state.selectedLectures.filter(lecture =>
                          !lecture.course.includes(courseCode.substring(0, 6)));
    this.setState({selectedLectures: updatedLectures});
  }

  // Method passed to child component CoursePanel to clear all the courses in selectedCourses.
  clearSelectedCourses() {
    this.setState({selectedCourses: []});
    this.setState({selectedLectures: []});
  }

  addSelectedLecture(courseCode, session, lectureCode, lectureTimes) {
    let updatedLectures = this.state.selectedLectures;
    // The maximum number of courses in the lecture list with the same code is 3, one for each session (F, S, Y)
    let index = this.state.selectedLectures.map(lecture => lecture.course).indexOf(courseCode);
    while (index != -1) {
      if (this.state.selectedLectures[index].session === session) {
        updatedLectures.splice(index, 1);
      }
      index = this.state.selectedLectures.map(lecture => lecture.course).indexOf(courseCode, index + 1);
    }
    let lectureSession = this.createNewCourse(courseCode, session, lectureCode, lectureTimes);
    updatedLectures.push(lectureSession);
    this.setState({selectedLectures: updatedLectures});
    console.log(this.state.selectedLectures)
  }

  removeSelectedLecture(courseCode, lectureSession) {
    let updatedLectures = this.state.selectedLectures;
    let index = updatedLectures.map(lecture => lecture.course).indexOf(courseCode);
    while (index != -1) {
      if (this.state.selectedLectures[index].session === lectureSession.session) {
        updatedLectures.splice(index, 1);
      }
      index = this.state.selectedLectures.map(lecture => lecture.course).indexOf(courseCode, index + 1);
    }
    updatedLectures.splice(index, 1);
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
      for(let i = 0; i< times[day].length; i+=2){
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
