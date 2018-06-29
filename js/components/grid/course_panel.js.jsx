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
        let course = {courseCode: "", F: [], S:[], Y:[]}
        course.courseCode = data.name;
        let fallLectures = this.parseLectures(data.fallSession.lectures);
        course.F = fallLectures.concat(this.parseLectures(data.fallSession.tutorials));

        let springLectures = this.parseLectures(data.springSession.lectures);
        course.S = springLectures.concat(this.parseLectures(data.springSession.tutorials));

        let yearLectures = this.parseLectures(data.yearSession.lectures);
        course.Y = yearLectures.concat(this.parseLectures(data.yearSession.tutorials));
        this.setState({courseInfo: course});
        console.log(this.state.courseInfo)
    });
  }

  parseLectures(lectures) {
    // Remove duplicated lecture sections
    let allLectures = lectures.filter((lecture, index, lectures) => {
      return (lectures.map(lect => lect.section).indexOf(lecture.section) == index)
    });
    let parsedLectures = [];

    let days = {0: 'M', 1: 'T', 2: 'W', 3: 'R', 4: 'F'};
    // Loop through the lecture sections to get each section's session code and lecture times
    for (let i=0; i < allLectures.length; i++) {
      // Check to make sure its not a restricted section. Restricted sections have enrollment
      // restricted for a particular group of students, but happens at the same time and place as a regular
      // lecture/tutorial section.
      if (allLectures[i].section.charAt(3) !== '2' && allLectures[i].times !== 'Online Web Version') {
        let lecture = {courseName: allLectures[i].code.substring(0, 6) + " (" + allLectures[i].section.substring(0,1) + ")",
                       lectureCode: allLectures[i].section.substring(0, 1) + allLectures[i].section.substring(3),
                       times: {}};
        let allTimes = allLectures[i].times;
        // The times in the original data are arrays of form [day, startTime], where day is an integer
        // between 0 and 4 that corresponds to days Monday to Friday. Eg: [0, 14] is a Monday lecture that
        // starts are 2PM. Every half hour is an individual array.
        // Loop through each individual time array and add the start time to the corresponding day.
        for (let j=0; j < allTimes.length; j++) {
          let day = days[(allTimes[j].timeField[0])];
          if (!lecture.times[day]) {
            lecture.times[day] = [allTimes[j].timeField[1]];
          }
          else {
            lecture.times[day].push(allTimes[j].timeField[1]);
          }
        }
        // Loop through each day of the week and remove the half hour increments.
        // Only keep start time and end time in the array.
        for (let weekday in lecture.times) {
          let times = []
          if (lecture.times.hasOwnProperty(weekday)) {
            times.push(lecture.times[weekday][0]);
            for (let hour=1; hour < lecture.times[weekday].length; hour++) {
              if (hour+1 >= lecture.times[weekday].length ||
                lecture.times[weekday][hour+1] != lecture.times[weekday][hour] + 0.5) {
                times.push(lecture.times[weekday][hour] + 0.5);
              }
            }
            lecture.times[weekday] = times;
          }
        }
        parsedLectures.push(lecture);
      }
    }
    parsedLectures.sort(function (lec1, lec2) {return lec1.lectureCode > lec2.lectureCode});
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
      <li key={this.props.courseCode} id={this.props.courseCode + "-li"} className={"ui-accordion ui-widget ui-helper-reset"}>
        <h3>
          <div className="icon-div">
              <img src="static/res/ico/delete.png" className="close-icon" onClick={this.removeCourse}/>
          </div>
          <div onClick={this.toggleSelect}>
            {this.props.courseCode}
          </div>
        </h3>
        { this.state.selected &&
          <div className="sections ui-accordion-content"
                id={"ui-accordion-" + this.props.courseCode + "-li-panel-0"}>
            <SectionList courseCode={this.props.courseCode}
                          section="Y"
                          lectures={this.state.courseInfo.Y}
                          addSelectedLecture={this.props.addSelectedLecture}
                          selectedLectures={this.props.selectedLectures}
                          removeSelectedLecture={this.props.removeSelectedLecture}/>
            <SectionList courseCode={this.props.courseCode}
                          section="F"
                          lectures={this.state.courseInfo.F}
                          addSelectedLecture={this.props.addSelectedLecture}
                          selectedLectures={this.props.selectedLectures}
                          removeSelectedLecture={this.props.removeSelectedLecture}/>
            <SectionList courseCode={this.props.courseCode}
                          section="S"
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
    const lectureSections = this.props.lectures.map(
      lecture => <LectureSection key={this.props.courseCode + lecture.lectureCode + this.props.section}
                                  section={this.props.section}
                                  courseCode={this.props.courseCode}
                                  lecture={lecture}
                                  addSelectedLecture={this.props.addSelectedLecture}
                                  selectedLectures={this.props.selectedLectures}
                                  removeSelectedLecture={this.props.removeSelectedLecture}/>)
    return(
      <ul className={"sectionList-" + this.props.section} id="lecture-list">
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
    let index = this.props.selectedLectures.map(lecture => lecture.course).indexOf(this.props.lecture.courseName);
    const selectedLecturesCodes = this.props.selectedLectures.map(lecture => lecture.lectureCode);

    while (index != -1 && (selectedLecturesCodes[index] !== this.props.lecture.lectureCode ||
          this.props.section !== this.props.selectedLectures[index].session)) {
      index = this.props.selectedLectures.map(lecture => lecture.course).indexOf(this.props.lecture.courseName, index + 1);
    }
    // If index != -1, then the while loop stopped because the lectureCode and section both matched
    (index != -1) ? this.props.removeSelectedLecture(this.props.lecture.courseName, this.props.lecture) :
                    this.props.addSelectedLecture(this.props.lecture.courseName, this.props.section,
                                                      this.props.lecture.lectureCode, this.props.lecture.times);
  }

  render() {
    return(
      <li id={this.props.courseCode + "-" + this.props.lecture.lectureCode + "-" + this.props.section}
          onClick={this.selectLecture}>
        {this.props.lecture.lectureCode}
      </li>
    )
  }
}
