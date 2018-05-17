export class SearchPanel extends React.Component {
  constructor(props) {
    super(props);
    this.state = { value: '' };
    // This binding is necessary to make `this` work in the callback
    this.handleInput = this.handleInput.bind(this);
  }

  handleInput(event) {
    this.setState({ value: event.target.value }, () => {
      console.log("Input: " + this.state.value);
    });
  }

  render() {
    return (
      // This div is all for the textbox input
      <div id="search-layout" className="col-md-2 col-xs-6">
        <div id="filter-container">
          <form onsubmit="return false;">
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
          <CourseList />
        </div>
      </div>
    );
  }
}

class CourseList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      courses: [],
      courseFilter: ''
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
      this.enableSearch();
      // searches through all of the courses in "data",
      // and stores each individual course code name
      // into 'courses' list
      let courses = data.split('\n').map(course => course.substring(0, 8));
      this.setState({ courses: courses });
    });
  }

  enableSearch() {
    // Whenever a key is released on "(#course-filter)", run this function ***
    $('#course-filter').keyup(() => {
      // Sets the state of this component such that it updates the
      // "courseFilter" attribute
      this.setState({ courseFilter: $('#course-filter').val().toUpperCase() });
    });
  }

  render() {
    let searchList = [];
    // If there are courses to be filtered
    if (this.state.courseFilter !== '') {
      // From the "courses" list, filter out elements based off of "courseFilter"
      searchList = this.state.courses.filter(
        course => course.indexOf(this.state.courseFilter) > -1
      ).map(course => <CourseEntry course={course} key={course} />
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


class CourseEntry extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      star: false
    };
  }

  // Inverts the 'star' boolean attribute in the state
  toggleStar() {
    this.setState({ star: !this.state.star });
  }

  render() {
    let classes = this.state.star ? 'starred-course' : '';
    return (
      <li id={this.props.course + '-search'}
          className={classes}
          onClick={this.toggleStar}>
        {this.props.course}
      </li>
    );
  }
}
