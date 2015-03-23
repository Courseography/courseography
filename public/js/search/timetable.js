var Timetable = React.createClass({
    getInitialState: function () {
        return {courses: [], search: ""};
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

        $('#filter').keyup(function() {
            this.updateFilter($('#filter').val());
        }.bind(this));
    },

    updateFilter: function(filter) {
        this.setState({search: filter});
    },

    render: function() {
        var search = this.state.search;
        var courseRows = this.state.courses.filter(function (course) {
            return course.name.indexOf(search) > -1 || 
                   (course.F !== null && course.F.lectures.some(function (lec) {
                       return lec.instructor.indexOf(search) > -1; })) ||
                   (course.S !== null && course.S.lectures.some(function (lec) {
                       return lec.instructor.indexOf(search) > -1; })) ||
                   (course.Y !== null && course.Y.lectures.some(function (lec) {
                       return lec.instructor.indexOf(search) > -1; }));
        }).map(function (course) {
            var fallLec = "";
            var springLec = "";

            var inName = course.name.indexOf(search) > -1;

            if (course.F !== null) {
                fallLec = course.F.lectures.filter(function (lec) {
                    return inName || lec.instructor.indexOf(search) > -1;
                }).map(function (lec) {
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
                springLec = course.S.lectures.filter(function (lec) {
                    return inName || lec.instructor.indexOf(search) > -1;
                }).map(function (lec) {
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
                    <td className="FOffering"><table className="courseTable"><tbody>{fallLec}</tbody></table></td>
                    <td className="SOffering"><table className="courseTable"><tbody>{springLec}</tbody></table></td>
                </tr>
            );
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
    <Timetable />,
    document.getElementById('timetableContainer'));
