var CourseCode = React.createClass({
    getInitialState: function() {
        return {selected: false}
    },

    updateSelected: function() {
        var selected = (getCookie(this.props.courseID) == 'active') ? true : false;
        this.setState({selected: selected});
    },

    toggleFullInfo: function() {
        $('#info').toggle();
    },

    render: function() {

        var categoryClasses = 'code';
        var courseClasses = 'full_name';

        if (this.state.selected) {
            categoryClasses += ' category_selected';
            courseClasses += ' course_selected';
        }

        return (
            <div id = {this.props.courseID} >
                <p className = {categoryClasses} onClick={this.toggleFullInfo}>  {this.props.courseID.toUpperCase() + "H"}  </p>
                <div id = 'info' className = 'more-info'>
                    <p className = {courseClasses}> {getCourseTitle(this.props.courseID)} </p>
                </div>
            </div>
           
        );
    }
})


React.render(<CourseCode courseID = 'csc108'/>, document.getElementById('spec_csc108'))