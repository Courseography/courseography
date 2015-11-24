var CourseCode = React.createClass({
    getInitialState: function() {
        return {
            selected: false,
            infoOpened: false
        }
    },

    componentWillMount: function() {
        this.setState({selected: getCookie(this.getIdName()) === 'active' ||
                                 getCookie(this.getIdName()) === 'overridden'});
    },

    toggleFullInfo: function() {
        this.setState({infoOpened: !this.state.infoOpened});
    },

    getCategoryName: function() {
        var categoryName = '';

        var editedCourseNames = this.props.courseIDs.map(function (course) {
            return course.toUpperCase() + 'H';
        });

        if (this.props.courseIDs[0] === 'mat135') {
            // special case for calculus requirement since it doesn't fit the same pattern
            return '(MAT135H and MAT136H) or MAT137Y or MAT157Y';
        } else { 
            return editedCourseNames.join(" or ");
        }
    },

    getIdName: function() {
        var idName = this.props.courseIDs[0].substring(0, 3);
        
        this.props.courseIDs.map(function (course) {
            idName += course.substring(3, 6);
        });

        // math and stats courses need extra stuff appended to their IDs 
        // (mainly to check if they are active or not through their cookie)
        if (this.props.courseIDs[0] === 'mat135') {
            idName += 'calc1';
        } else if (this.props.courseIDs[0] === 'mat221') {
            idName += 'lin1';
        } else if (this.props.courseIDs[0] === 'sta247') {
            idName += 'sta1';
        }

        return idName;
    },

    render: function() {

        var classes = 'course';
        var infoClasses = 'more-info';

        if (this.state.selected) {
            classes += " selected";
        }

        if (this.state.infoOpened) {
            infoClasses += ' info_opened'
        }

        return (
            <div id ={this.getIdName()} className={classes}>
                <p className="code" onClick={this.toggleFullInfo}> {this.getCategoryName()} </p>
                <div id = {this.props.courseIDs[0] + '_info'} className={infoClasses}>
                    {this.props.courseIDs.map(function (course) {
                         return <p className="full_name"> {getCourseTitle(course)} </p>
                    })}
                </div>
            </div>
        );
    }
})

var MultipleCourseCode = React.createClass({
    getInitialState: function() {
        return {
            completed: false,
            completedTextBoxes: 0,
            infoOpened: false
        }
    },

    componentDidMount: function() {
        this.setState({completedTextBoxes: this.state.completedTextBoxes + this.props.courses.length},
            this.checkIfCompleted);
    },
    
    toggleFullInfo: function() {
        this.setState({infoOpened: !this.state.infoOpened});
    },

    checkIfCompleted: function() {
        this.setState({completed: this.state.completedTextBoxes === this.props.textBoxNumber});
    },

    handleKeyDown: function(e) {
        if (e.keyCode === 13) {
            if (this.state.completedTextBoxes <= this.props.textBoxNumber + 1) {
                if (e.target.defaultValue === '' && e.target.value !== '') {
                    this.setState({completedTextBoxes: this.state.completedTextBoxes + 1},
                        this.checkIfCompleted);
                } else if (e.target.defaultValue !== '' && e.target.value === '') { 
                    this.setState({completedTextBoxes: this.state.completedTextBoxes - 1},
                        this.checkIfCompleted);
                }
            }

            e.target.defaultValue = e.target.value;
        }  
    },

    render: function() {

        var me = this;
        var classes = 'course';
        var infoClasses = 'more-info';

        if (this.state.completed) {
            classes += ' selected';
        }

        if (this.state.infoOpened) {
            infoClasses += ' info_opened'
        }

        return (
            <div id={this.props.courseID} className={classes}>
                <p className="code" onClick={this.toggleFullInfo}> {this.props.categoryName} </p>
                <div id = {'spec' + this.props.courseID.substring(5, this.props.courseID.length)} className={infoClasses}>
                    <p className="full_name"> 
                        {Array.apply(0, Array(this.props.textBoxNumber)).map(function (x, i) {
                            if (me.props.textboxesDisabled) {
                                return <input type='text' defaultValue={me.props.courses[i]} onKeyDown={me.handleKeyDown} disabled />;
                            } else {
                                return <input type='text' defaultValue={me.props.courses[i]} onKeyDown={me.handleKeyDown} />;
                            }
                        })}
                    </p>
                </div>
            </div>
        );
    }
})

var CourseYear = React.createClass({
    getInitialState: function() {
        return {};
    },

    render: function() {
        return (
            <div>
                <h2> {this.props.yearName} </h2>
                {this.props.courses.map(function (courses) {
                    return <CourseCode id={courses[0]} courseIDs={courses} />;
                })}
            </div>
        );
    }
})

var SpecialistPost = React.createClass({
    getInitialState: function() {
        return {
            selected: true,
            activeCourses: this.updateActiveCourses()
        }
    },

    componentWillMount: function() {
        this.setState({selected: getCookie('specialist') === 'active'});
    },

    isLevel400: function (course, level400Array) {
        return notSpecialistCourse(course) && course.substring(3, 4) === '4' && level400Array.length < 3;
    },

    isLevel300: function (course, level300Array) {
        return notSpecialistCourse(course) && course.substring(3, 4) >= '3' && level300Array.length < 3;
    },

    isLevelExtra: function (course, levelExtraArray) {
        return notSpecialistCourse(course) && course.substring(3, 4) >= '3' && levelExtraArray.length < 4;
    },

    isInquiryCourse: function(course, inquiryArray) {
        return CSCinq.indexOf(course) >= 0 && inquiryArray < 1;
    },

    getCourses: function () {
        // [level400Courses, level300Courses, levelExtraCourses, inquiryCourse]
        var courseArrays = [];
        // currently this.isInquiryCourse is considered mutually exclusive to other categories
        // - this will change eventually.
        var courseChecks = [this.isLevel400, this.isLevel300, this.isLevelExtra, this.isInquiryCourse];

        this.state.activeCourses.map(function (course) {
            for (var i = 0; i < courseChecks.length; i++) {
                if (courseArrays.length <= i) {
                    courseArrays.push([]);
                }
                 
                if (courseChecks[i](course, courseArrays[i])) {
                    courseArrays[i].push(course);
                    break;
                }
            }
        });

        return courseArrays;
    },

    updateActiveCourses: function() {
        var activeCourses = [];

        // Check for active CSC courses
        for (var i = 0; i < allCourses.length; i++) {
            if (getCookie(allCourses[i].toLowerCase()) === 'active' ||
                getCookie(allCourses[i].toLowerCase()) === 'overridden') {
                activeCourses.push(allCourses[i]);
            }
        }

        // Check for active math courses
        for (var i = 0; i < math.length; i++) {
            if (getCookie(math[i].toLowerCase()) === 'active') {
                activeCourses.push(math[i]);
            }
        }

        return activeCourses;
    },


    render: function() {

        var firstYearCourses = [['csc108'], ['csc148'], ['csc165', 'csc240'], ['mat135', 'mat136']];
        var secondYearCourses = [['csc207'], ['csc209'], ['csc236', 'csc240'], ['csc258'], ['csc263', 'csc265'], ['mat221', 'mat223', 'mat240'], 
                                ['sta247', 'sta255', 'sta257']];
        var laterYearCourses = [['csc369'], ['csc373']];

        var courseCategoryArrays = this.getCourses();

        return (
            <div id="specialist_window">
                <CourseYear yearName='First Year' courses={firstYearCourses} />
                <CourseYear yearName='Second Year' courses={secondYearCourses} />
                <CourseYear yearName='Later Years' courses={laterYearCourses} />
                <MultipleCourseCode courseID='spec_400' textBoxNumber={3} courses={courseCategoryArrays[0]} textboxesDisabled={true}
                    categoryName='Any 400-level CSC course, BCB410H, BCB420H, BCB430Y, ECE489H (1.5 FCEs)' />
                <MultipleCourseCode courseID='spec_300' textBoxNumber={3} courses={courseCategoryArrays[1]} textboxesDisabled={true}
                    categoryName='Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.5 FCEs)' />
                <MultipleCourseCode courseID="spec_extra" textBoxNumber={4} courses={courseCategoryArrays[2]} textboxesDisabled={false}
                    categoryName='Any of the following: 300+ level CSC course; MAT: 235/237/257, any 300+ 
                                  except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H; 
                                  BCB: 410H/420H/430Y (2.0 FCEs)' />
                <MultipleCourseCode courseID="spec_inq" textBoxNumber={1} courses={courseCategoryArrays[3]} textboxesDisabled={true}
                    categoryName='Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H, 
                    CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H, or PEY (0.5 FCEs) 
                    ** Note: Type "PEY" for Check my POSt to recognize it **' />
                <h2> Notes </h2>
                <p id='notes'> - No more than 1.0 FCE from CSC490H1, CSC491H1, CSC494H1, CSC495H1, BCB430Y1 may be used to fulfill program requirements </p>
            </div>
        );
    }
})


React.render(<SpecialistPost />, document.getElementById('div_specialist'));
