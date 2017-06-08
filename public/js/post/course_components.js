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

    getCategory: function() {
        var categoryName = '';
        var me = this;

        var editedCourseNames = [];

        // since you can render an array with a combination of jsx elements and strings, this combines all span elements and
        // "or" strings in between to be easily rendered later
        this.props.courseIDs.forEach(function(course) {
            editedCourseNames.push(<span id={course} className='courseName' onClick={me.openModal}>{course.toUpperCase() + 'H'}</span>);
            editedCourseNames.push('or');
        })

        editedCourseNames.pop();

        if (this.props.courseIDs[0] === 'mat135') {
            // special case for calculus requirement since it doesn't fit the same pattern
            return (
                <p className='code'>
                    (<span id='mat135' className='courseName' onClick={this.openModal}>MAT135H</span> and
                    <span id='mat136' className='courseName' onClick={this.openModal}>MAT136H</span>) or
                    <span id='mat137' className='courseName' onClick={this.openModal}>MAT137Y</span>or
                    <span id='mat157' className='courseName' onClick={this.openModal}>MAT157Y</span>
                 </p>
            )
        } else {
            return <p className='code'> {editedCourseNames} </p>;
        }
    },

    getIdName: function() {
        var idName = this.props.courseIDs[0].substring(0, 3);

        this.props.courseIDs.forEach(function (course) {
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

    getTitle: function(id) {
        if (id == 'mat137' || id == 'mat157') {
            var course = new Course(id + 'Y1')
        } else {
            var course = new Course(id + 'H1');
        }

        return id.toUpperCase() + ": " + course.title;
    },

    openModal: function(e) {
        this.props.openModal(e.target.id);
    },

    render: function() {
        var me = this;
        var classes = '';
        var id = this.getIdName();

        if (this.state.selected) {
            classes += ' selected';
        }

        if (this.state.infoOpened) {
            classes += ' info_opened';
        }

        return (
            <div id={this.getIdName()} className={classes}>
                {this.getCategory()}
            </div>
        );
    }
})


export var MultipleCourseCode = React.createClass({
    getInitialState: function() {
        return {
            completed: false,
            completedTextBoxes: 0,
            infoOpened: false,
            textboxValues: this.createInitialValueArray()
        }
    },

    componentWillMount: function() {
        this.setState({completedTextBoxes: this.state.completedTextBoxes +
                                           this.props.courses.length},
                      this.checkIfCompleted);
    },

    toggleFullInfo: function() {
        this.setState({infoOpened: !this.state.infoOpened});
    },

    checkIfCompleted: function() {
        this.setState({completed: this.state.completedTextBoxes === this.props.textBoxNumber});
    },

    createInitialValueArray: function() {
        var array = this.props.courses.slice();
        for (var i = this.props.courses.length; i < this.props.textBoxNumber; i++) {
            array.push('');
        }
        return array;
    },

    isValidExtraCourse: function(course) {
        var validCourseCodes = ['CSC', 'MAT', 'STA', 'ECE', 'BCB'];
        return validCourseCodes.indexOf(course.substring(0, 3)) > -1 &&
               course.length === 6;
    },

    handleOnChange: function(e) {
        var newValues = this.state.textboxValues.slice();
        var oldCourse = newValues[e.target.id];
        var newCourse = e.target.value.substring(0, 6);
        newValues[e.target.id] = newCourse;

        var numCompleted = this.countCompletedTextBoxes();

        this.setState({textboxValues: newValues}, function () {
            this.setState({
                completedTextBoxes: numCompleted,
                completed: numCompleted === this.props.textBoxNumber});
        });

        if (this.isValidExtraCourse(oldCourse) &&
            !(this.isValidExtraCourse(newCourse))) {
            this.props.changeCourseCredit(-0.5);
        } else if (this.isValidExtraCourse(newCourse) &&
            !(this.isValidExtraCourse(oldCourse))) {
            this.props.changeCourseCredit(0.5);
        }
    },

    countCompletedTextBoxes: function() {
        return this.state.textboxValues.filter((v) => {
            return this.isValidExtraCourse(v);
        }).length;
    },

    render: function() {
        var me = this;
        var classes = '';

        if (this.state.completed) {
            classes += ' selected';
        }

        if (this.state.infoOpened) {
            classes += ' info_opened'
        }

        var courseID = this.props.courseID;

        return (
            <div id={courseID} className={classes}>
                <p className='code' onClick={this.toggleFullInfo}>
                    <span>{this.props.categoryName}</span>
                </p>
                <div id = {'spec' + this.props.courseID.substring(5, this.props.courseID.length)} className='more-info'>
                    <p className="full_name">
                        {Array.apply(0, Array(this.props.textBoxNumber)).map(function (x, i) {
                            if (me.isValidExtraCourse(me.state.textboxValues[i].substring(0, 6))) {
                                var className = 'valid_extra_course';
                            } else {
                                var className = 'not_valid_extra_course';
                            }

                            return (
                                <input type='text'
                                       key={i}
                                       id={i}
                                       className={className}
                                       value={me.state.textboxValues[i].substring(0, 6)}
                                       onChange={me.handleOnChange}
                                       disabled={me.props.textboxesDisabled} />
                            );
                        })}
                    </p>
                </div>
            </div>
        );
    }
})


export var CourseCategory = React.createClass({
    render: function() {
        return (
            <div>
                <h2>{this.props.yearName}</h2>
                {this.props.courses.map((courses) =>
                        <CourseCode id={courses[0]}
                                    key={courses[0]}
                                    courseIDs={courses}
                                    openModal={this.props.openModal} />
                )}
            </div>
        );
    }
})


export var InquiryCategory = React.createClass({
    getInitialState: function() {
        return {
            completed: false,
            infoOpened: false,
            value: this.getDefaultCourse()
        }
    },

    componentWillMount: function() {
        this.setState({completed: this.props.course !== ''});
    },

    toggleFullInfo: function() {
        this.setState({infoOpened: !this.state.infoOpened});
    },

    getDefaultCourse: function() {
        return this.props.course;
    },

    handleOnChange: function(e) {
        var newValue = e.target.value;
        this.setState({value: newValue}, function () {
            if (this.state.value === 'PEY' || CSCinq.indexOf(this.state.value) >= 0) {
                this.setState({completed: true});
            } else {
                this.setState({completed : false});
            }
        });
    },

    render: function() {
        var classes = '';

        if (this.state.completed) {
            classes += ' selected';
        }

        if (this.state.infoOpened) {
            classes += ' info_opened'
        }

       return (
            <div id={this.props.courseID} className={classes}>
                <p className='code' onClick={this.toggleFullInfo}>
                    <span>{this.props.categoryName}</span>
                </p>
                <div id={'spec' + this.props.courseID.substring(5, this.props.courseID.length)}
                     className='more-info'>
                    <p className='full_name'>
                        <input type='text'
                               value={this.state.value}
                               onChange={this.handleOnChange} />
                    </p>
                </div>
            </div>
        );
    }
})
