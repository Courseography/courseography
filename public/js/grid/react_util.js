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
    render: function() {
        return (
            <div>
                <div id="filter-container">
                    <form onsubmit="return false;">
                        <input id="course-filter" className="form-control" placeholder="Enter a course!" autoComplete="off" type="text" />
                    </form>
                </div>
                <div id="search-container">
                    <CourseList />
                </div>
            </div>
        );
    }
});

var CourseList = React.createClass({
    getInitialState: function() {
        return {
            courses: [], courseFilter: ''
        };
    },

    componentDidMount: function() {
        $.ajax({
            url: 'all-courses',
            dataType: 'text',
            success: function (data) {
                this.enableSearch();
                courses = data.split('\n').map(function (course) {
                    return course.substring(0, 8);
                });
                this.setState({courses: courses});
            }.bind(this)
        });
    },

    enableSearch: function() {
        'use strict';

        $('#course-filter').keyup(function() {
            this.setState({courseFilter: $('#course-filter').val().toUpperCase()});
        }.bind(this));
    },

    render: function() {

        var state = this.state;

        if (state.courseFilter !== '') {
            var searchList = state.courses.filter(function (course) {
                return course.indexOf(state.courseFilter) > -1;
            }).map(function (course) {
                return <CourseEntry course={course} />
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

        var star = '';

        if (this.state.star) {
            star += 'starred-course';
        }

        return (
            <li id={this.props.course + '-search'} className={star} onClick={this.toggleStar}>{this.props.course}</li>
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


React.render(
    <CoursePanel />,
    document.getElementById('course-select-wrapper'));

React.render(
    <SearchPanel />,
    document.getElementById('search-layout'));

React.render(
    <InfoPanel />,
    document.getElementsByClassName('col-md-8 col-xs-12 col-md-pull-2')[0]);
