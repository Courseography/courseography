export class CoursePanel extends React.Component {
  render() {
    const courses = this.props.selectedCourses.map(
      course => <Course key={course} courseCode={course} removeCourse={this.props.removeCourse}/>)

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
    this.state = {
      selected: false
    }
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
            <SectionList courseCode={this.props.courseCode} section="Y" lectures={["L0101", "L0102"]}/>
            <SectionList courseCode={this.props.courseCode} section="F" lectures={["L0101", "L0105"]}/>
            <SectionList courseCode={this.props.courseCode} section="S" lectures={["L5101"]}/>
          </div>
        }
      </li>
    )
  }
}

class SectionList extends React.Component {
  render() {
    const lectureSections = this.props.lectures.map(
      lecture => <LectureSection key={this.props.courseCode + "-" + lecture + "-" + this.props.section}
                                  section={this.props.section}
                                  courseCode={this.props.courseCode}
                                  lectureCode={lecture}/>)

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
    this.toggleLecture = this.toggleLecture.bind(this);
    this.state = {
      lectureSelected: false
    }
  }

  toggleLecture() {
    this.setState({lectureSelected: !this.state.selected});
  }

  render() {
    return(
      <li id={this.props.courseCode + "-" + this.props.lectureCode + "-" + this.props.section}
          onClick={this.toggleLecture}>
        {this.props.lectureCode}
      </li>
    )
  }
}
