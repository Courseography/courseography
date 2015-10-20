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

var MultipleCourseCode = React.createClass({
    getInitialState: function() {
        return {
            completed: false,
            completedTextBoxes: 0
        }
    },
    
    toggleFullInfo: function() {
          $('#' + this.props.courseID + ' > .more-info').toggle();
    },

    checkIfCompleted: function() {
       this.setState({completed: this.state.completedTextBoxes === this.props.data.textBoxNumber});
    },

    handleKeyDown: function(e) {
        if (e.keyCode === 13) {
            if (this.state.completedTextBoxes < this.props.data.textBoxNumber) {
                 this.setState({completedTextBoxes: this.state.completedTextBoxes +=1})
            }

            this.checkIfCompleted();
        }  
    },

    render: function() {

        var categoryClasses = 'code';
        var courseClasses = 'full_name';

        if (this.state.completed) {
            categoryClasses += ' category_selected';
            courseClasses += ' course_selected';
        }
        
        var me = this;

        return (
            <div id={this.props.courseID} className='course'>
                <p className = {categoryClasses} onClick={this.toggleFullInfo}> {this.props.data.categoryName} </p>
                <div className='more-info'>
                    <p className = {courseClasses}> 
                        {Array.apply(0, Array(this.props.data.textBoxNumber)).map(function (x, i) {
                            return <input type='text' onKeyDown={me.handleKeyDown} />;
                        })}
                    </p>
                </div>
            </div>
        );
    }
})


var SpecialistPost = React.createClass({
    getInitialState: function() {
        return {
            selected: true
        }
    },

    componentWillMount: function() {
        this.setState({selected: getCookie('specialist') === "active"});
    },

    render: function() {

        var firstYearCourses = ['CSC108', 'CSC148', 'CSC165', 'MAT135'];
        var secondYearCourses = ['CSC207', 'CSC209', 'CSC236', 'CSC258', 'CSC263', 'MAT221', 'STA247'];
        var laterYearCourses = ['CSC369', 'CSC373'];

        return (
            <div id="specialist_window">
                <h2> First Year </h2>
                {firstYearCourses.map(function (course) {
                    return <CourseCode courseID={course.toLowerCase()} />;
                })}
                <h2> Second Year </h2>
                {secondYearCourses.map(function (course) {
                    return <CourseCode courseID={course.toLowerCase()} />;
                })}
                <h2> Later Years </h2>
                {laterYearCourses.map(function (course) {
                    return <CourseCode courseID={course.toLowerCase()} />;
                })}
                <MultipleCourseCode courseID="spec400" data={{textBoxNumber: 3, 
                    categoryName: "Any 400-level CSC course, BCB410H, BCB420H, BCB430Y, ECE489H (1.5 FCEs)"}} />
                <MultipleCourseCode courseID="spec300" data={{textBoxNumber: 3, 
                    categoryName: "Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.5 FCEs)"}} />
                <MultipleCourseCode courseID="specextra" data={{textBoxNumber: 4, 
                    categoryName: "Any of the following: 300+ level CSC course; MAT: 235/237/257, any 300+ \
                                     except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H; \
                                     BCB: 410H/420H/430Y (2.0 FCEs)"}} />
                <p className="code"> Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H, 
                    CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H, or PEY (0.5 FCEs) 
                    ** Note: Type 'PEY' for Check my POSt to recognize it ** </p>
                <h2> Notes </h2>
                <p id='notes'> - No more than 1.0 FCE from CSC490H1, CSC491H1, CSC494H1, CSC495H1, BCB430Y1 may be used to fulfill program requirements </p>
            </div>
        );
    }
})


React.render(<SpecialistPost />, document.getElementById('div_specialist'));
