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

var SpecialistPost = React.createClass({
    getInitialState: function() {
        return {
            selected: true,
            courses: ['CSC108', 'CSC148', 'CSC165', 'MAT135', 'CSC207',
                      'CSC209', 'CSC236', 'CSC258', 'CSC263',
                      'MAT221', 'STA247', 'CSC369', 'CSC373']
        }
    },

    componentWillMount: function() {
        this.setState({selected: getCookie('specialist') === "active"});
    },

    render: function() {
        return (
            <div id="specialist_div">
                {this.state.courses.map(function (course) {
                    return <CourseCode courseID={course.toLowerCase()} />;
                })}
                <p className="code"> Any 400-level CSC course, BCB410H, BCB420H, BCB430Y, ECE489H (1.5 FCEs) </p>
                <p className="code"> Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.5 FCEs) </p>
                <p className="code"> Any of the following: 300+ level CSC course; MAT: 235/237/257, any 300+
                                     except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H;
                                     BCB: 410H/420H/430Y (1.5 FCEs) </p>
                <p className="code"> Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H, 
                    CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H, or PEY (0.5 FCEs) 
                    ** Note: Type 'PEY' for Check my POSt to recognize it ** </p>
            </div>
        );
    }
})


React.render(<SpecialistPost />, document.getElementById('div_specialist'));
