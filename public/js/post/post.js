var CourseCode = React.createClass({
    getInitialState: function() {
        return {selected: false}
    },

    componentWillMount: function() {
        this.setState({selected: getCookie(this.props.courseID) === 'active'});
    },

    toggleFullInfo: function() {
        $('#' + this.props.courseID + '_info').toggle();
    },

    render: function() {

        var categoryClasses = 'code';
        var courseClasses = 'full_name';

        if (this.state.selected) {
            categoryClasses += ' category_selected';
            courseClasses += ' course_selected';
        }

        return (
            <div id={this.props.courseID} className="course">
                <p className={categoryClasses} onClick={this.toggleFullInfo}>  {this.props.courseID.toUpperCase() + "H"}  </p>
                <div id={this.props.courseID + '_info'} className='more-info'>
                    <p className={courseClasses}> {getCourseTitle(this.props.courseID)} </p>
                </div>
            </div>
        );
    }
})


React.render(<CourseCode courseID = 'csc108'/>, document.getElementById('spec_csc108'))
