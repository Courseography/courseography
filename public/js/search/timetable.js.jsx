var filterCourse = function (inst, time, lec) {
    'use strict';
    return lec.instructor.indexOf(inst) > -1 &&
           (time.length < 2 || hasTime(time, lec.time));
};


var hasTime = function (timeStr, times) {
    'use strict';
    var time = ['MTWRF'.indexOf(timeStr[0]) + '',
                timeStr.substr(1) + '-0'];
    for (var i = 0; i < times.length; i++) {
        if (time[0] === times[i][0] && time[1] === times[i][1]) {
            return true;
        }
    }
    return false;
};


var Search = React.createClass({
    getInitialState: function () {
        return {curDept: '', depts: []};
    },

    componentDidMount: function () {
        $.ajax({
            url: 'depts',
            dataType: 'json',
            success: function(data) {
                this.setState({depts: data});
            }.bind(this),
            error: function(xhr, status, err) {
                console.error('course-info', status, err.toString());
            }.bind(this)
        });
    },

    updateDept: function () {
        var selectedDept = React.findDOMNode(this.refs.deptSelect).value;
        this.setState({curDept: selectedDept});
        this.refs.timetable.populateTable(selectedDept);
    },

    render: function () {
        var options = this.state.depts.map(function(dept) {
            return (<option value={dept}>{dept}</option>);
        });

        return (
            <div id="search">
            <div id="timetableSearch">
                <h2>2015-2016 Timetable Search</h2>
                <div id="searchOptions">
                <label htmlFor="deptSelect">Dept:</label>
                <select ref="deptSelect" name="dept" onChange={this.updateDept} id="deptSelect">
                    <option value="none" selected="selected">---</option>
                    {options}
                </select>
                <br />
                <label htmlFor="codeFilter">Code:</label>
                <input type="text" className="text-input" id="codeFilter" placeholder="CSC108" />
                <br />
                <label htmlFor="instFilter">Instructor:</label>
                <input type="text" className="text-input" id="instFilter" placeholder="Liu" />
                <br />
                <label htmlFor="timeFilter">Time:</label>
                <input type="text" className="text-input" id="timeFilter" placeholder="M10" />
                </div>
            </div>
            <div id="timetableContainer">
                <Timetable dept={this.state.curDept} ref="timetable" />
            </div>
            </div>
        );
    }
});


var Timetable = React.createClass({
    getInitialState: function () {
        return {courses: [], codeSearch: '', instSearch: '', timeSearch: ''};
    },

    componentDidMount: function() {
        $('#codeFilter').keyup(function() {
            this.setState({codeSearch: $('#codeFilter').val()});
        }.bind(this));
        $('#instFilter').keyup(function() {
            this.setState({instSearch: $('#instFilter').val()});
        }.bind(this));
        $('#timeFilter').keyup(function() {
            this.setState({timeSearch: $('#timeFilter').val()});
        }.bind(this));
    },

    populateTable: function(dept) {
        $.ajax({
            url: 'course-info',
            data: {dept: dept},
            dataType: 'json',
            success: function(data) {
                this.setState({courses: data});
            }.bind(this),
            error: function(xhr, status, err) {
                console.error('course-info', status, err.toString());
            }.bind(this)
        });
    },

    render: function() {
        var state = this.state;
        var courseRows = this.state.courses.filter(function (course) {
            var lecs = course.fallSession.lectures
                                         .concat(course.springSession.lectures)
                                         .concat(course.yearSession.lectures);

            return course.name.indexOf(state.codeSearch) > -1 &&
                   lecs.some(function (lec) {
                       return filterCourse(state.instSearch, state.timeSearch, lec);
                   });
        }).map(function (course) {
            if (course.yearSession.lectures.length === 0) {
                var fallLec = '';
                var springLec = '';

                fallLec = course.fallSession.lectures.filter(function (lec) {
                    return filterCourse(state.instSearch, state.timeSearch, lec);
                }).map(function (lec) {
                    return (
                        <tr>
                        <td className="timetableSection">
                            {lec.section.substring(0, 1) + lec.section.substring(4)}
                        </td>
                        <td className="timetableTime">{lec.timeStr}</td>
                        <td className="timetableInstructor">{lec.instructor}</td>
                        <td className="timetableCap">{lec.cap}</td>
                        <td className="timetableWait">{lec.wait}</td>
                        </tr>);
                });

                springLec = course.springSession.lectures.filter(function (lec) {
                    return filterCourse(state.instSearch, state.timeSearch, lec);
                }).map(function (lec) {
                    return (
                        <tr>
                        <td className="timetableSection">
                            {lec.section.substring(0, 1) + lec.section.substring(4)}
                        </td>
                        <td className="timetableTime">{lec.timeStr}</td>
                        <td className="timetableInstructor">{lec.instructor}</td>
                        <td className="timetableCap">{lec.cap}</td>
                        <td className="timetableWait">{lec.wait}</td>
                        </tr>);
                });

                return (
                    <tr>
                        <td className="timetableCourseName">{course.name}</td>
                        <td className="FOffering"><table className="courseTable"><tbody>{fallLec}</tbody></table></td>
                        <td className="SOffering"><table className="courseTable"><tbody>{springLec}</tbody></table></td>
                    </tr>
                );
            } else {
                var yearLec = course.yearSession.lectures.filter(function (lec) {
                        return filterCourse(state.instSearch, state.timeSearch, lec);
                    }).map(function (lec) {
                        return (
                            <tr>
                            <td className="timetableSection">
                                {lec.section.substring(0, 1) + lec.section.substring(4)}
                            </td>
                            <td className="timetableTime">{lec.time_str}</td>
                            <td className="timetableInstructor">{lec.instructor}</td>
                            <td className="timetableCap">{lec.cap}</td>
                            <td className="timetableWait">{lec.wait}</td>
                            </tr>);
                    });

                return (
                    <tr>
                        <td className="timetableCourseName">{course.name}</td>
                        <td colSpan="2" className="YOffering"><table className="courseTable"><tbody>{yearLec}</tbody></table></td>
                    </tr>
                );
            }
        });

        return (
            <table id="timetableMain">
            <thead>
            <tr>
            <th className="timetableCourseName">Courses</th>
            <th className="sessionHeader FOffering">Fall</th>
            <th className="sessionHeader SOffering">Spring</th>
            </tr>
            </thead>
            <tbody>
            <tr>
            <td className="timetableCourseName"></td>
            <td className="FOffering">
                <table className="courseTable"><thead><tr>
                    <th className="timetableSection">Sec</th>
                    <th className="timetableTime">Time</th>
                    <th className="timetableInstructor">Instructor</th>
                    <th className="timetableCap">Cap</th>
                    <th className="timetableWait">Wait</th>
                </tr></thead></table>
            </td>
            <td className="SOffering">
                <table className="courseTable"><thead><tr>
                    <th className="timetableSection">Sec</th>
                    <th className="timetableTime">Time</th>
                    <th className="timetableInstructor">Instructor</th>
                    <th className="timetableCap">Cap</th>
                    <th className="timetableWait">Wait</th>
                </tr></thead></table>
            </td>
            </tr>
            {courseRows}
            </tbody>
            </table>
        );
    }
});


React.render(
    <Search />,
    document.getElementById('content'));
