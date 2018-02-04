// CoursePanel is a React Component which simply renders the "Clear All" text
var CoursePanel = React.createClass({
    render: function() {
        return (
           // Creates an id/className for this section (no functionality I don't think)
            <ul className="trapScroll-enabled" id="course-select" onClick={() => alert("test")}>
                <li id="clear-all">
                    <h3>Clear All</h3>
                </li>
            </ul>
        );
    }
});


// Component for the SearchPanel
var SearchPanel = React.createClass({

    render: function() {
        return (
            // This div is all for the textbox input
            <div>
                <div id="filter-container">
                    <form onsubmit="return false;">
                        <input id="course-filter" className="form-control" placeholder="Enter a course!" autoComplete="off" type="text" value={this.state.value}/>
                        this.handleInput();
                    </form>
                </div>

                <div id="search-container">
                    <CourseList  />
                </div>
            </div>
        );



    },
    // handleInput: function() {
    //   console.log("Input: " + this.state.value);
    // }
});

var CourseList = React.createClass({
    // constructor(props) {
    //   super(props);
    //   this.state = {
    //     value: this.props.value
    //   };
    // },

    // Function which returns the courses (empty list) and any course filters (blank for now)
    getInitialState: function() {
        return {
            courses: [], courseFilter: ''
        };
    },

    componentDidMount: function() {
        // AJAX requests allow the programmer to:
        //    1. update a webpage without refreshing
        //    2. Request data from a server AFTER the webpage is loaded
        //    3. Receive data from a server AFTER the webpage is loaded
        //    4. Send data to the server - in the background

        // I believe this makes an ajax call to retrieve courses from the database***
        $.ajax({
            // url to which the ajax request is sent to
            url: 'all-courses',
            // data which you're expective to receive back from the server
            dataType: 'text',

            // note: "this" refers to "data"
            success: function (data) {
                // searches through all of the courses in "data",
                // and stores each individual course code name
                // into 'courses' list
                this.enableSearch();
                var courses = data.split('\n').map(function (course) {
                    return course.substring(0, 8);
                });

                // sets the state of this React component, which will
                // ultimately be rendered in SearchPanel
                this.setState({courses: courses});

            // Bind ensures that "this" refers to "data". ***
            }.bind(this)
        });
    },


    enableSearch: function() {
        // ***
        'use strict';
        // Whenever a key is released on "(#course-filter)", run this function ***
        $('#course-filter').keyup(function() {
            // Sets the state of this component such that it updates the
            // "courseFilter" attribute
            this.setState({courseFilter: $('#course-filter').val().toUpperCase()});
        }.bind(this));
    },


    render: function() {
        // Stores the state
        var state = this.state;
        // If there are courses to be filtered
        if (state.courseFilter !== '') {
            // What is "course" thats being passed? ***
            // From the "courses" list, filter out elements based off of "courseFilter"
            var searchList = state.courses.filter(function (course) {
                // ***
                return course.indexOf(state.courseFilter) > -1;

            // Render the following tag, with input corresponding to the
            // filtered course list
            }).map(function (course) {
                return <CourseEntry course={course} key={course} />
            });
        }

        // Return all the unfiltered courses in the "courses" list in a <ul>
        return (
            <div id="search-list">
                <ul>{searchList}</ul>
            </div>
        );
    }
});


var CourseEntry = React.createClass({
    getInitialState: function() {
        return {
            star: false
        };
    },

    // Inverts the 'star' boolean variable in the state
    toggleStar: function() {
        this.setState({star: !this.state.star});
    },

    render: function() {
        // presumably for a "star course" feature.
        var classes = '';
        if (this.state.star) {
            classes += 'starred-course';
        }
        // Lists out all the unfiltered courses, and gives the option for
        // the user to 'star' them ***
        return (
            <li id={this.props.course + '-search'} className={classes} onClick={this.toggleStar}>{this.props.course}</li>
        );
    }
});


var InfoPanel = React.createClass({
    render: function() {
        return (
            <div id="info" className="row"></div>
        );
    }
});


export function initGrid() {
  ReactDOM.render(
       <CoursePanel />,
      document.getElementById('course-select-wrapper'));

  ReactDOM.render(
      <SearchPanel />,
      document.getElementById('search-layout'));

  // ReactDOM.render(
  //     <InfoPanel />,
  //     document.getElementsByClassName('col-md-8 col-xs-12 col-md-pull-2')[0]);
}
