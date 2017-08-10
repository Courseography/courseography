var CoursePanel = React.createClass({
    getInitialState: function() {
        return {
            courseRoster: [["1", [], []], ["2", ["2-1"], ["2-1", "2-2"]], ["3", ["3-1", "3-2"], ["3-1"]]]
        };
    },

    clearCourseRoster: function() {
       this.setState({courseRoster: []});
    },

    render: function() {
        var courseList = [];

        for (let i = 0; i < this.state.courseRoster.length; i++) {
            courseList.push(<CourseInformation courseCode={this.state.courseRoster[i][0]} />);
        }

        return (
            <ul className="trapScroll-enabled" id="course-select">
                <li id="clear-all" onClick={this.clearCourseRoster}>
                    <h3>Clear All</h3>
                </li>
                {courseList}
            </ul>
        );
    }
});

var CourseInformation = React.createClass({
    getInitialState: function() {
        return {
            satisfied: false
        };
    },

    toggleSatisfied: function() {
        this.setState({satisfied: !this.state.satisfied});
    },

    render: function() {
        if(this.state.satisfied === false) {
            return (
                <li id={this.props.courseCode + "-li"}
                    className="ui-accordion ui-widget ui-helper-reset"
                    role="tablist">
                    <div className="ui-accordion-header ui-helper-reset ui-state-default ui-corner-all ui-accordion-icons"
                         onClick={this.toggleSatisfied}
                         role="tab"
                         id={"ui-accordion-" + this.props.courseCode + "-li-header-0"}
                         aria-controls={"ui-accordion-" + this.props.courseCode + "-li-panel-0"}
                         aria-selected="false"
                         aria-expanded="false"
                         tabIndex="0">
                        <span className="ui-accordion-header-icon ui-icon ui-icon-triangle-1-e"></span>
                        <div className="icon-div">
                            <img src="static/res/ico/delete.png" className="close-icon" />
                        </div>
                        <h3 taken="true"
                            satisfied="true">
                            {this.props.courseCode}
                        </h3>
                    </div>
                </li>
            );
        } else {
            return (
                <li id={this.props.courseCode + "-li"} className="ui-accordion ui-widget ui-helper-reset" role="tablist" onClick={this.toggleSatisfied}>{this.props.courseCode}</li>
            );
        }
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

    courseListRefWrapper: function() {
        return (
            this.refs.courseListRef.enableSearch()
        );
    },

    render: function() {
        return (
            <div>
                <div id="filter-container">
                    <form>
                        <input id="course-filter" className="form-control" placeholder="Enter a course!" autoComplete="off" type="text" onKeyUp={this.courseListRefWrapper}/>
                    </form>
                </div>
                <div id="search-container">
                    {/* onKeyUp is a prop, so the only way that I know how to
                        set a prop is below like I have done with courses, but
                        I also don't know how to access enableSearch in
                        CourseList so I can set it to onKeyUp.*/}
                    <CourseList courses={this.state.courseList} ref="courseListRef" />
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
        this.setState({courseFilter: $('#course-filter').val().toUpperCase()});
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
