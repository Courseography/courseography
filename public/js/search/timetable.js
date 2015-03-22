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
                        <td class="timetableSection">{lec.section}</td>
                        <td class="timetableTime">{lec.timeStr}</td>
                        <td class="timetableInstructor">{lec.instructor}</td>
                        <td class="timetableCap">{lec.cap}</td>
                        <td class="timetableWait">{lec.wait}</td>
                        </tr>);
                });
            }

            if (course.S !== null) {
                springLec = course.S.lectures.map(function (lec) {
                    return (
                        <tr>
                        <td class="timetableSection">{lec.section}</td>
                        <td class="timetableTime">{lec.timeStr}</td>
                        <td class="timetableInstructor">{lec.instructor}</td>
                        <td class="timetableCap">{lec.cap}</td>
                        <td class="timetableWait">{lec.wait}</td>
                        </tr>);
                });
            }

            return (
                <tr>
                    <td class="timetableCourseName">{course.name}</td>
                    <td class="FOffering"><table class="courseTable">{fallLec}</table></td>
                    <td class="SOffering"><table class="courseTable">{springLec}</table></td>
                </tr>
            );
        });
        return (
            <table>
            <tr>
            <th class="timetableCourseName">Courses</th>
            <th class="sessionHeader FOffering">Fall</th>
            <th class="sessionHeader SOffering">Spring</th>
            </tr>
            <tr>
            <td class="timetableCourseName"></td>
            <td class="FOffering">
                <table class="courseTable"><tbody><tr>
                    <th class="timetableSection">Sec</th>
                    <th class="timetableTime">Time</th>
                    <th class="timetableInstructor">Instructor</th>
                    <th class="timetableCap">Cap</th>
                    <th class="timetableWait">Wait</th>
                </tr></tbody></table>
            </td>
            <td class="SOffering">
                <table class="courseTable"><tbody><tr>
                    <th class="timetableSection">Sec</th>
                    <th class="timetableTime">Time</th>
                    <th class="timetableInstructor">Instructor</th>
                    <th class="timetableCap">Cap</th>
                    <th class="timetableWait">Wait</th>
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
