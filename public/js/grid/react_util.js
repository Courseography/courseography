var CoursePanel = React.createClass({
    render: function() {
        return (
            <ul className="trapScroll-enabled" id="course-select">
                <li id="clear-all">
                    <h3>Clear All</h3>
                </li>
            </ul>
        );
    }
});

var SearchPanel = React.createClass({
    getInitialState: function() {
        return {
            courseList: []
        };
    },

    componentDidMount: function() {
        $.ajax({
            url: 'all-courses',
            dataType: 'text',
            success: function (data) {
                var courses = data.split('\n').map(function (course) {
                    return course.substring(0, 8);
                });
                this.setState({courseList: courses});
            }.bind(this)
        });
    },

    render: function() {
        return (
            <div>
                <div id="filter-container">
                    <form>
                        <input id="course-filter" className="form-control" placeholder="Enter a course!" autoComplete="off" type="text" />
                    </form>
                </div>
                <div id="search-container">
                    <CourseList courses={this.state.courseList}/>
                </div>
            </div>
        );
    }
});

var CourseList = React.createClass({
    getInitialState: function() {
        return {
            courseFilter: ''
        };
    },

    enableSearch: function() {
        'use strict';

        $('#course-filter').keyup(function() {
            this.setState({courseFilter: $('#course-filter').val().toUpperCase()});
        }.bind(this));
    },

    componentDidMount: function() {
        this.enableSearch();
    },

    render: function() {
        var state = this.state;

        if (state.courseFilter !== '') {
            var searchList = this.props.courses.filter(function (course) {
                return course.indexOf(state.courseFilter) > -1;
            }).map(function (course) {
                return <CourseEntry course={course} key={course} />
            });
        }

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

    toggleStar: function() {
        this.setState({star: !this.state.star});
    },

    render: function() {

        var classes = '';

        if (this.state.star) {
            classes += 'starred-course';
        }

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
