export class CoursePanel extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const courses = this.props.selectedCourses.map(
                course => <Course courseCode={course} removeCourse={this.props.removeCourse}/>)

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
    this.handleSelect = this.handleSelect.bind(this);
    this.removeCourse = this.removeCourse.bind(this);
    this.state = {
      selected: false
    }
  }

  handleSelect() {
    this.setState({selected: !this.state.selected})
  }

  removeCourse() {
    this.props.removeCourse(this.props.courseCode);
  }


  render() {
    //Why cant directly use onClick={this.props.removeCourse(this.props.courseCode)} in button
    return (
      <li key={this.props.courseCode} id={this.props.courseCode} >
        <h3>
          <button onClick={this.removeCourse}>
            <img src="static/res/ico/delete.png" className="close-icon"/>
          </button>
          <div onClick={this.handleSelect}>
          {this.props.courseCode}
          </div>
        </h3>
        { this.state.selected &&
          <h5> Some lecture </h5>
        }
      </li>
    )
  }
}
