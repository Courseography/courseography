var CoursePanel = React.createClass({
    getInitialState: function() {
        return {
            courseRoster: [["CLS000", [], [], []], ["CLS001", ["LEC1000"], ["LEC1001"], ["LEC1002"]], ["CLS002", [], ["LEC2000", "LEC2001"], ["LEC2002"]]]
        };
    },

    clearCourseRoster: function() {
       this.setState({courseRoster: []});
    },

    removeCourse: function(i) {
       console.log("Removing course at index " + i + ".");
       i = parseInt(i, 10);
       var newRoster = [];

       for (let s = 0; s < i; s++) {
           newRoster.push(this.state.courseRoster[s]);
       }

       for (let s = i + 1; s < this.state.courseRoster.length; s++) {
           newRoster.push(this.state.courseRoster[s]);
       }

       this.setState({courseRoster: newRoster});
    },

    render: function() {
        var courseList = [];

        for (let i = 0; i < this.state.courseRoster.length; i++) {
            courseList.push(<CourseInformation courseCode={this.state.courseRoster[i][0]} yLectures={this.state.courseRoster[i][1]} fLectures={this.state.courseRoster[i][2]} sLectures={this.state.courseRoster[i][3]} key={i} cat={i} removeCourse={this.removeCourse}/>);
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
        console.log("toggleSatisfied method of CourseInformation component with prop courseCode as " + this.props.courseCode + " has been called.");
        this.setState({satisfied: !this.state.satisfied});
    },

    render: function() {
        console.log("Rendering CourseInformation component with courseCode prop courseCode as " + this.props.courseCode + ".");
        var courseCodeProp = this.props.courseCode;
        var yLectureArray = [];
        var fLectureArray = [];
        var sLectureArray = [];

        for (let i = 0; i < this.props.yLectures.length; i++) {
            yLectureArray.push(<Lecture courseCode={courseCodeProp} lectureCode={this.props.yLectures[i]} key={i}/>);
        }

        for (let i = 0; i < this.props.fLectures.length; i++) {
            fLectureArray.push(<Lecture courseCode={courseCodeProp} lectureCode={this.props.fLectures[i]} key={i}/>);
        }

        for (let i = 0; i < this.props.sLectures.length; i++) {
            sLectureArray.push(<Lecture courseCode={courseCodeProp} lectureCode={this.props.sLectures[i]} key={i}/>);
        }

        if(this.state.satisfied === false) {
            return (
                <li id={courseCodeProp + "-li"}
                    className="ui-accordion ui-widget ui-helper-reset"
                    role="tablist">
                    <div className="ui-accordion-header ui-helper-reset ui-state-default ui-corner-all ui-accordion-icons"
                        onClick={this.toggleSatisfied}
                        role="tab"
                        id={"ui-accordion-" + courseCodeProp + "-li-header-0"}
                        aria-controls={"ui-accordion-" + courseCodeProp + "-li-panel-0"}
                        aria-selected="false"
                        aria-expanded="false"
                        tabIndex="0">
                        <span className="ui-accordion-header-icon ui-icon ui-icon-triangle-1-e"></span>
                        <div className="icon-div">
                            <img src="static/res/ico/delete.png" className="close-icon" onClick={() => this.props.removeCourse(this.props.cat)}/>
                        </div>
                        <h3 taken="true"
                            satisfied="true">
                            {courseCodeProp}
                        </h3>
                    </div>
                    <div className="sections ui-accordion-content ui-helper-reset ui-widget-content ui-corner-bottom"
                        id={"ui-accordion-" + courseCodeProp + "-li-panel-0"}
                        aria-labelledby={"ui-accordion-" + courseCodeProp + "-li-header-0"}
                        role="tabpanel"
                        aria-hidden="true"
                        style={{display: "none"}}>
                        <ul className="sectionList-Y">
                            {yLectureArray}
                        </ul>
                        <ul className="sectionList-F">
                            {fLectureArray}
                        </ul>
                        <ul className="sectionList-S">
                            {sLectureArray}
                        </ul>
                    </div>
                </li>
            );
        } else {
            return (
                <li id={courseCodeProp + "-li"}
                    className="ui-accordion ui-widget ui-helper-reset"
                    role="tablist">
                    <div className="ui-accordion-header ui-helper-reset ui-state-default ui-corner-all ui-accordion-icons"
                        onClick={this.toggleSatisfied}
                        role="tab"
                        id={"ui-accordion-" + courseCodeProp + "-li-header-0"}
                        aria-controls={"ui-accordion-" + courseCodeProp + "-li-panel-0"}
                        aria-selected="true"
                        aria-expanded="true"
                        tabIndex="0">
                        <span className="ui-accordion-header-icon ui-icon ui-icon-triangle-1-e"></span>
                        <div className="icon-div">
                            <img src="static/res/ico/delete.png" className="close-icon" onClick={() => this.props.removeCourse(this.props.cat)}/>
                        </div>
                        <h3 taken="true"
                            satisfied="true">
                            {courseCodeProp}
                        </h3>
                    </div>
                    <div className="sections ui-accordion-content ui-helper-reset ui-widget-content ui-corner-bottom ui-accordion-content-active"
                        id={"ui-accordion-" + courseCodeProp + "-li-panel-0"}
                        aria-labelledby={"ui-accordion-" + courseCodeProp + "-li-header-0"}
                        role="tabpanel"
                        aria-hidden="false"
                        style={{display: "block"}}>
                        <ul className="sectionList-Y">
                            {yLectureArray}
                        </ul>
                        <ul className="sectionList-F">
                            {fLectureArray}
                        </ul>
                        <ul className="sectionList-S">
                            {sLectureArray}
                        </ul>
                    </div>
                </li>
            );
        }
    }
});

var Lecture = React.createClass({
    render: function() {
        return (
            <li id={this.props.courseCode + "-" + this.props.lectureCode}
                clicked="false"
                satisfied="true">
                {this.props.lectureCode}
            </li>
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
