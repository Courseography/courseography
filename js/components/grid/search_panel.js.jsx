/*
 * Creates a search box and stores the current user input that is in the search box.
 */
import React from 'react';


 export class SearchPanel extends React.Component {
  constructor(props) {
    super(props);
    this.state = { value: '' };
    // This binding is necessary to make `this` work in the callback
    this.handleInput = this.handleInput.bind(this);
  }

  handleInput(event) {
    this.setState({ value: event.target.value });
  }

  render() {
    return (
      // This div is all for the textbox input
      <div id="search-layout" className="col-md-2 col-xs-6">
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
      </div>
    );
  }
}

/*
 * Filters the database of courses based on the user's input in the search box and
 * generates a list of courses with course codes containing the user input in the search box.
 */
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

/*
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
    }
    else {
      this.props.selectCourse(this.props.course);
    }
  }

  render() {
    let classes = this.props.selectedCourses.indexOf(this.props.course) != -1 ? 'starred-course' : '';
    return (
      <li id={this.props.course + '-search'}
          className={classes}
          onClick={this.select}>
        {this.props.course}
      </li>
    );
  }
}
