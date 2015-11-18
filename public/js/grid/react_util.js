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
            courses: [],
            searchList: []
        };
    },

    componentDidMount: function() {
        $.ajax({
            url: "all-courses",
            dataType: "text",
            async: false,
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

        var self = this;

        $('#course-filter').keyup(function() {
            self.resetSearchList();
        });
    },

    resetSearchList: function() {
        'use strict';

        var filter = $('#course-filter').val().toUpperCase();

        if (filter !== '') {
            var searchList = this.state.courses.filter(function (course) {
                return course.indexOf(filter) > -1;
            }).map(function (course) {
                return <CourseEntry course={course} />
            });
        }
        this.setState({searchList: searchList});
    },

    render: function() {
        return (
            <div id="search-list">
                <ul>{this.state.searchList}</ul>
            </div>
        );
    }
});

var CourseEntry = React.createClass({
    render: function() {
        return (
            <li id={this.props.course + '-search'}>{this.props.course}</li>
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
