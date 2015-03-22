var Timetable = React.createClass({
    getInitialState: function () {
        return {courses: []};
    },
    componentDidMount: function() {
        $.ajax({
            url: 'course-info',
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
        var courseRows = this.state.courses.map(function (course) {
            var fallLec = "";
            var springLec = "";

            if (course.F !== null) {
                fallLec = course.F.lectures.map(function (lec) {
                    return (
                        <tr>
                        <td className="timetableSection">{lec.section}</td>
                        <td className="timetableTime">{lec.timeStr}</td>
                        <td className="timetableInstructor">{lec.instructor}</td>
                        <td className="timetableCap">{lec.cap}</td>
                        <td className="timetableWait">{lec.wait}</td>
                        </tr>);
                });
            }

            if (course.S !== null) {
                springLec = course.S.lectures.map(function (lec) {
                    return (
                        <tr>
                        <td className="timetableSection">{lec.section}</td>
                        <td className="timetableTime">{lec.timeStr}</td>
                        <td className="timetableInstructor">{lec.instructor}</td>
                        <td className="timetableCap">{lec.cap}</td>
                        <td className="timetableWait">{lec.wait}</td>
                        </tr>);
                });
            }

            return (
                <tr>
                    <td className="timetableCourseName">{course.name}</td>
                    <td className="FOffering"><table className="courseTable">{fallLec}</table></td>
                    <td className="SOffering"><table className="courseTable">{springLec}</table></td>
                </tr>
            );
        });
        return (
            <table id="timetableMain">
            <tr>
            <th className="timetableCourseName">Courses</th>
            <th className="sessionHeader FOffering">Fall</th>
            <th className="sessionHeader SOffering">Spring</th>
            </tr>
            <tr>
            <td className="timetableCourseName"></td>
            <td className="FOffering">
                <table className="courseTable"><tbody><tr>
                    <th className="timetableSection">Sec</th>
                    <th className="timetableTime">Time</th>
                    <th className="timetableInstructor">Instructor</th>
                    <th className="timetableCap">Cap</th>
                    <th className="timetableWait">Wait</th>
                </tr></tbody></table>
            </td>
            <td className="SOffering">
                <table className="courseTable"><tbody><tr>
                    <th className="timetableSection">Sec</th>
                    <th className="timetableTime">Time</th>
                    <th className="timetableInstructor">Instructor</th>
                    <th className="timetableCap">Cap</th>
                    <th className="timetableWait">Wait</th>
                </tr></tbody></table>
            </td>
            </tr>
            {courseRows}
            </table>
        );
    }
});

React.render(
    <Timetable />,
    document.getElementById('timetableContainer'));
