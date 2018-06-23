export class CoursePanel extends React.Component {
  render() {
    const courses = this.props.selectedCourses.map(
      course => <Course key={course}
                        selectedLectures={this.props.selectedLectures}
                        courseCode={course}
                        removeCourse={this.props.removeCourse}
                        addSelectedLecture={this.props.addSelectedLecture}
                        removeSelectedLecture={this.removeSelectedLecture}/>)

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
    this.state = {
      selected: false
    }
  }

  componentDidMount() {
    fetch(
      'course?name=' + this.props.courseCode, // url to which the AJAX request is sent to
    )
    .then(response => response.json()) //datatype
    .then(data => {
        console.log(data)
        let course = {courseCode: "", F: [], S:[], Y:[]}
        course.courseCode = data.name;

        let fallLectures = this.parseLectures(data.fallSession.lectures);
        fallLectures.push(this.parseLectures(data.fallSession.tutorials));

        let springLectures = this.parseLectures(data.springSession.lectures);
        fallLectures.push(this.parseLectures(data.springSession.tutorials));

        let yearLectures = this.parseLectures(data.yearSession.lectures);
        yearLectures.push(this.parseLectures(data.yearSession.tutorials));

        course.F = fallLectures;
        course.S = springLectures;
        course.Y = yearLectures;
    });
  }

  parseLectures(lectures) {
    // Remove duplicated lecture sections
    lectures = lectures.filter((lecture, index, lectures) => {
      return lectures.map(lect => lect.section).indexOf(lecture.section) == index
    });
    let parsedLectures = [];

    let days = {0: 'M', 1: 'T', 2: 'W', 3: 'R', 4: 'F'};
    // Loop through the lecture sections to get each section's session code and lecture times
    for (let i=0; i < lectures.length; i++) {
      let lec = {lectureCode: lectures[i].section, times: {}};
      let timeArray = lectures[i].times;
      // The times in the original data are arrays of form [day, startTime], where day is an integer
      // between 0 and 4 that corresponds to days Monday to Friday. Eg: [0, 14] is a Monday lecture that 
      // starts are 2PM. Every half hour is an individual array. 
      // Loop through each individual time array and add the start time to the corresponding day.  
      for (let j=0; j < timeArray.length; j++) {
        let day = days[(timeArray[j].timeField[0])];
        if (!lec.times[day]) {
          lec.times[day] = [timeArray[j].timeField[1]];
        }
        else {
          lec.times[day].push(timeArray[j].timeField[1]);
        }
      }
      // Loop through each day of the week and remove the half hour increments.
      // Only keep start time and end time in the array. 
      for (let weekday in lec.times) {
        let times = []
        if (lec.times.hasOwnProperty(weekday)) {
          times.push(lec.times[weekday][0]);
          for (let hour=1; hour < lec.times[weekday].length; hour++) {
            if (hour+1 >= lec.times[weekday].length || 
              lec.times[weekday][hour+1] != lec.times[weekday][hour] + 0.5) {
              times.push(lec.times[weekday][hour] + 0.5);
            }
          }
          lec.times[weekday] = times;
        }
      }
      parsedLectures.push(lec);
    }
    console.log(parsedLectures);
    return parsedLectures;
  }

  toggleSelect() {
    this.setState({selected: !this.state.selected})
  }

  removeCourse() {
    this.props.removeCourse(this.props.courseCode);
  }

  render() {
    return (
      <li key={this.props.courseCode} id={this.props.courseCode + "-li"} >
        <h3>
          <div className="icon-div">
              <img src="static/res/ico/delete.png" className="close-icon" onClick={this.removeCourse}/>
          </div>
          <div onClick={this.toggleSelect}>
            {this.props.courseCode}
          </div>
        </h3>
        { this.state.selected &&
          <div className="sections ui-accordion-header"
                id={"ui-accordion-" + this.props.courseCode + "-li-panel-0"}>
            <SectionList courseCode={this.props.courseCode}
                          section="Y"
                          lectures={this.props.courseSections.Y}
                          addSelectedLecture={this.props.addSelectedLecture}
                          selectedLectures={this.props.selectedLectures}
                          removeSelectedLecture={this.removeSelectedLecture}/>
            <SectionList courseCode={this.props.courseCode}
                          section="F"
                          lectures={this.props.courseSections.F}
                          addSelectedLecture={this.props.addSelectedLecture}
                          selectedLectures={this.props.selectedLectures}
                          removeSelectedLecture={this.removeSelectedLecture}/>
            <SectionList courseCode={this.props.courseCode}
                          section="S" lectures={this.props.courseSections.S}
                          addSelectedLecture={this.props.addSelectedLecture}
                          selectedLectures={this.props.selectedLectures}
                          removeSelectedLecture={this.removeSelectedLecture}/>
          </div>
        }
      </li>
    )
  }
}

class SectionList extends React.Component {
  render() {
    // Remove duplicates from the sessions array generated by the Course object.
    // Note: this was used to parse information from the deprecated Course object
    let lectures = this.props.lectures;
    lectures = lectures.filter((lecture, index, lectures) => {
      return lectures.map(lect => lect.id).indexOf(lecture.id) == index
    });
    const lectureSections = lectures.map(
      lecture => <LectureSection key={lecture.id}
                                  section={this.props.section}
                                  courseCode={this.props.courseCode}
                                  lectureCode={lecture.name}
                                  lecture={lecture}
                                  addSelectedLecture={this.props.addSelectedLecture}
                                  selectedLectures={this.props.selectedLectures}
                                  removeSelectedLecture={this.removeSelectedLecture}/>)
    return(
      <ul className={"sectionList-" + this.props.section} id="lecture-list">
        {lectureSections}
      </ul>
    )
  }
}

class LectureSection extends React.Component {
  // constructor(props) {
  //   super(props);
  //   this.toggleLecture = this.toggleLecture.bind(this);
  //   this.state = {
  //     lectureSelected: false
  //   }
  // }

  // toggleLecture() {
  //   this.setState({lectureSelected: !this.state.selected});
  // }

  constructor(props) {
    super(props);
    this.selectLecture = this.selectLecture.bind(this);
  }

  // Check whether the course is already in the selectCourses list.
  // Remove the course if it is, or add the course if it is not.
  selectLecture() {
    this.props.selectedLectures.indexOf(this.props.lecture) != -1 ? this.props.removeSelectedLecture(this.props.lecture) :
                        this.props.addSelectedLecture(this.props.lecture);
  }

  render() {
    return(
      <li id={this.props.lecture.id}
          onClick={this.selectLecture}>
        {this.props.lectureCode}
      </li>
    )
  }
}
