var Course = React.createClass({

    render: function() {

        var fallLectures = this.props.data.fallSession.lectures.map(function (section) {
            return <Section section={section} />
        });

        var springLectures = this.props.data.springSession.lectures.map(function (section) {
            return <Section section={section} />
        });

        var yearLectures = this.props.data.yearSession.lectures.map(function (section) {
            return <Section section={section} />
        });

        return (
            <div>
                <div>{fallLectures}</div>
                <div>{springLectures}</div>
                <div>{yearLectures}</div>
            </div>
        );
    }
});


var Section = React.createClass({

    getInitialState: function() {
        return {
            clicked: false, satisfied: true
        };
    },

    render: function() {

        var section = this.props.section;
        var id = section.name + '-' + section.section + '-' + section.session;

        return (
            <div>
                <li id={id} clicked={this.state.clicked} satisfied={this.state.satisfied}>
                    <div data-custom-attribute="instructor">{section.instructor}</div>
                    <div data-custom-attribute="cap">{section.cap}</div>
                    <div data-custom-attribute="enrol">{section.enrol}</div>
                    <div data-custom-attribute="wait">{section.wait}</div>
                </li>
            </div>
        );
    }
});


var getCourse = function(courseName) {

    'use strict';

    var course;

    $.ajax({
        url: 'course',
        dataType: 'json',
        data: {name : courseName},
        async: false,

        success: function (data) {
            React.render(
                <Course data={data} />,
                    document.getElementById('course-select-wrapper'));
        },

        error: function () {
            throw 'No course file';
        }
    });
}
