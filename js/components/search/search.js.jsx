import React from 'react';
import ReactDOM from 'react-dom';


function filterCourse(inst, time, lec) {
    return lec.instructor.indexOf(inst) > -1 &&
           (time.length < 2 || hasTime(time, lec.time));
};


function hasTime(timeStr, times) {
    var time = ['MTWRF'.indexOf(timeStr[0]) + '',
                timeStr.substr(1) + '-0'];
    for (var i = 0; i < times.length; i++) {
        if (time[0] === times[i][0] && time[1] === times[i][1]) {
            return true;
        }
    }
    return false;
};


class Search extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            curDept: '',
            depts: []
        }
        this.updateDept = this.updateDept.bind(this);
    }

    componentDidMount() {
        $.ajax({
            url: 'depts',
            dataType: 'json',
            success: (data) => {
                this.setState({depts: data});
            },
            error: (xhr, status, err) => {
                console.error('course-info', status, err.toString());
            }
        });
    }

    updateDept() {
        var selectedDept = ReactDOM.findDOMNode(this.refs.deptSelect).value;
        this.setState({curDept: selectedDept});
        this.refs.timetable.populateTable(selectedDept);
    }

    render() {
        var options = this.state.depts.map((dept) => {
            return (<option key={dept} value={dept}>{dept}</option>);
        });

        return (
            <div id="search">
            <div id="timetableSearch">
                <h2>Timetable Search</h2>
                <div id="searchOptions">
                <label htmlFor="deptSelect">Dept:</label>
                <select
                    ref="deptSelect"
                    name="dept"
                    onChange={this.updateDept}
                    id="deptSelect"
                    defaultValue="none">
                    <option value="none">---</option>
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
}


class Timetable extends React.Component {
    constructor(props) {
        super(props);
        this.state = {courses: [], codeSearch: '', instSearch: '', timeSearch: ''};
        this.populateTable = this.populateTable.bind(this);
    }

    componentDidMount() {
        $('#codeFilter').keyup(() => {
            this.setState({codeSearch: $('#codeFilter').val()});
        });
        $('#instFilter').keyup(() => {
            this.setState({instSearch: $('#instFilter').val()});
        });
        $('#timeFilter').keyup(() => {
            this.setState({timeSearch: $('#timeFilter').val()});
        });
    }

    populateTable(dept) {
        $.ajax({
            url: 'course-info',
            data: {dept: dept},
            dataType: 'json',
            success: (data) => {
                this.setState({courses: data});
            },
            error: (xhr, status, err) => {
                console.error('course-info', status, err.toString());
            }
        });
    }

    render() {
        var state = this.state;
        var courseRows = this.state.courses.filter((course) => {
            var lecs = course.fallSession.lectures
                                         .concat(course.springSession.lectures)
                                         .concat(course.yearSession.lectures);

            return course.name.indexOf(state.codeSearch) > -1 &&
                   lecs.some(({meetingData}) => filterCourse(state.instSearch, state.timeSearch, meetingData));
        }).map((course) => {
            if (course.yearSession.lectures.length === 0) {
                var fallLec = '';
                var springLec = '';

                fallLec = course.fallSession.lectures.filter(({meetingData}) => {
                    return filterCourse(state.instSearch, state.timeSearch, meetingData);
                }).map(({meetingData}) => {
                    return (
                        <tr>
                        <td className="timetableSection">
                            {meetingData.section}
                        </td>
                        <td className="timetableTime">{meetingData.timeStr}</td>
                        <td className="timetableInstructor">{meetingData.instructor}</td>
                        <td className="timetableCap">{meetingData.cap}</td>
                        <td className="timetableWait">{meetingData.wait}</td>
                        </tr>);
                });

                springLec = course.springSession.lectures.filter(({meetingData}) => {
                    return filterCourse(state.instSearch, state.timeSearch, meetingData);
                }).map(({meetingData}) => {
                    return (
                        <tr key={meetingData.section}>
                        <td className="timetableSection">
                            {meetingData.section}
                        </td>
                        <td className="timetableTime">{meetingData.timeStr}</td>
                        <td className="timetableInstructor">{meetingData.instructor}</td>
                        <td className="timetableCap">{meetingData.cap}</td>
                        <td className="timetableWait">{meetingData.wait}</td>
                        </tr>);
                });

                return (
                    <tr key={course.name}>
                        <td className="timetableCourseName">{course.name}</td>
                        <td className="FOffering"><table className="courseTable"><tbody>{fallLec}</tbody></table></td>
                        <td className="SOffering"><table className="courseTable"><tbody>{springLec}</tbody></table></td>
                    </tr>
                );
            } else {
                var yearLec = course.yearSession.lectures.filter(({meetingData}) => {
                        return filterCourse(state.instSearch, state.timeSearch, meetingData);
                    }).map(({meetingData}) => {
                        return (
                            <tr key={meetingData.section}>
                            <td className="timetableSection">
                                {meetingData.section}
                            </td>
                            <td className="timetableTime">{meetingData.time_str}</td>
                            <td className="timetableInstructor">{meetingData.instructor}</td>
                            <td className="timetableCap">{meetingData.cap}</td>
                            <td className="timetableWait">{meetingData.wait}</td>
                            </tr>);
                    });

                return (
                    <tr key={course.name}>
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
}


ReactDOM.render(
    <Search />,
    document.getElementById('content'));
