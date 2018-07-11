import React from 'react';


export class CoursePanel extends React.Component {
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
          <li id="clear-all" key="clear-all-grid" onClick={this.props.clearCourses}>
            <h3>Clear All</h3>
          </li>
          {courses}
        </ul>
      </div>
    );
  }
}

class Course extends React.Component {
  constructor(props) {
    super(props);
    this.toggleSelect = this.toggleSelect.bind(this);
    this.removeCourse = this.removeCourse.bind(this);
    this.parseLectures = this.parseLectures.bind(this);
    this.selectInfo = this.selectInfo.bind(this);
    this.modal = null;
    this.setModalRef = this.setModalRef.bind(this);
    this.state = {
      selected: false,
      courseInfo: {}
    }
  }

  componentDidMount() {
    fetch(
      'course?name=' + this.props.courseCode, // url to which the AJAX request is sent to
    )
    .then(response => response.json()) //datatype
    .then(data => {
        let course = {
          courseCode: "",
          F: [],
          S:[],
          Y:[]
        };
        course.courseCode = data.name;
        course.F = course.F.concat(this.parseLectures(data.fallSession.lectures),
                                    this.parseLectures(data.fallSession.tutorials));
        course.S = course.S.concat(this.parseLectures(data.springSession.lectures),
                                    this.parseLectures(data.springSession.tutorials));
        course.Y = course.Y.concat(this.parseLectures(data.yearSession.lectures),
                                    this.parseLectures(data.yearSession.tutorials));
        this.setState({courseInfo: course});
    });
  }

  parseLectures(lectures) {
    // Remove duplicated lecture sections
    let allLectures = lectures.filter((lecture, index, lectures) => {
      return (lectures.map(lect => lect.section).indexOf(lecture.section) === index)
    });
    let parsedLectures = [];

    let days = {0: 'M', 1: 'T', 2: 'W', 3: 'R', 4: 'F'};
    // Loop through the lecture sections to get each section's session code and lecture times
    allLectures.forEach( lectureInfo => {
      // Check to make sure its not a restricted section. Restricted sections have enrollment
      // restricted for a particular group of students, but happens at the same time and place as a regular
      // lecture/tutorial section.
      if (lectureInfo.section.charAt(3) !== '2' && lectureInfo.times !== 'Online Web Version') {
        let lecture = {
          courseName: lectureInfo.code.substring(0, 6) + " (" + lectureInfo.section.substring(0,1) + ")",
          lectureCode: lectureInfo.section.substring(0, 1) + lectureInfo.section.substring(3),
          times: {}
        };
        let allTimes = lectureInfo.times;
        // The times in the original data are arrays of form [day, startTime], where day is an integer
        // between 0 and 4 that corresponds to days Monday to Friday. Eg: [0, 14] is a Monday lecture that
        // starts are 2PM. Every half hour is an individual array.
        // Loop through each individual time array and add the start time to the corresponding day.
        allTimes.forEach( time => {
          let day = days[(time.timeField[0])];
          if (!lecture.times[day]) {
            lecture.times[day] = [time.timeField[1]];
          }
          else {
            lecture.times[day].push(time.timeField[1]);
          }
        });
        // Loop through each day of the week and remove the half hour increments.
        // Only keep start time and end time in the array.
        for (let weekday in lecture.times) {
          let times = []
          if (lecture.times.hasOwnProperty(weekday)) {
            times.push(lecture.times[weekday][0]);
            for (let hour = 1; hour < lecture.times[weekday].length; hour++) {
              if (hour+1 >= lecture.times[weekday].length ||
                lecture.times[weekday][hour+1] !== lecture.times[weekday][hour] + 0.5) {
                times.push(lecture.times[weekday][hour] + 0.5);
              }
            }
            lecture.times[weekday] = times;
          }
        }
        parsedLectures.push(lecture);
      }
    });
    parsedLectures.sort((lec1, lec2) => {return lec1.lectureCode > lec2.lectureCode});
    return parsedLectures;
  }

  toggleSelect() {
    this.setState({selected: !this.state.selected})
  }

  removeCourse() {
    this.props.removeCourse(this.props.courseCode);
  }

  setModalRef(element) {
    this.modal = element;
  }

  selectInfo() {
    this.modal.openModal(this.state.courseInfo.courseCode.substring(0, 6));
  }

  render() {
    return (
      <li key={this.props.courseCode} id={this.props.courseCode + "-li"} className={"ui-accordion ui-widget ui-helper-reset"}>
        <div className="ui-accordion-header ui-helper-reset ui-state-default ui-accordion-icons ui-accordion-header-active ui-state-active ui-corner-top"
              id={"ui-accordion-" + this.props.courseCode + "-li-header-0"}>
          <Modal ref={this.setModalRef}/>
          <div className="icon-div">
              <img src="static/res/ico/delete.png" className="close-icon" onClick={this.removeCourse}/>
              <img src="static/res/ico/about.png" className="close-icon" onClick={this.selectInfo}/>
          </div>
          <h3 onClick={this.toggleSelect}>
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

class LectureSection extends React.Component {
  constructor(props) {
    super(props);
    this.selectLecture = this.selectLecture.bind(this);
  }

  // Check whether the course is already in the selectCourses list.
  // Remove the course if it is, or add the course if it is not.
  selectLecture() {
    let sameLecture = this.props.selectedLectures.filter((lecture) => {
      return lecture.course === this.props.lecture.courseName && lecture.session === this.props.session &&
        lecture.lectureCode === this.props.lecture.lectureCode
    });
    // If sameLecture is not an empty array, then this lecture is already selected and should be removed
    if (sameLecture.length > 0) {
      this.props.removeSelectedLecture(this.props.lecture.courseName, this.props.session);
    } else {
      this.props.addSelectedLecture(this.props.lecture.courseName, this.props.session,
                                    this.props.lecture.lectureCode, this.props.lecture.times);
    }
  }

  render() {
    return(
      <li id={this.props.courseCode + "-" + this.props.lecture.lectureCode + "-" + this.props.session}
          onClick={this.selectLecture}>
        {this.props.lecture.lectureCode}
      </li>
    )
  }
}
