import React from 'react';


class CourseCode extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            selected: false,
            infoOpened: false
        }
        this.toggleFullInfo = this.toggleFullInfo.bind(this);
        this.getCategory = this.getCategory.bind(this);
        this.getIdName = this.getIdName.bind(this);
    }

    componentWillMount() {
        this.setState({selected: localStorage.getItem(this.getIdName()) === 'active' ||
                                 localStorage.getItem(this.getIdName()) === 'overridden'});
    }

    toggleFullInfo() {
        this.setState({infoOpened: !this.state.infoOpened});
    }

    getCategory() {
        var categoryName = '';

        var editedCourseNames = [];

        // since you can render an array with a combination of jsx elements and strings, this combines all span elements and
        // "or" strings in between to be easily rendered later
        this.props.courseIDs.forEach((course, i) => {
            editedCourseNames.push(
                <span id={course} key={i} className='courseName'
                    onClick={(e) => this.props.openModal(e.target.id)}>
                    {course.toUpperCase() + 'H'}
                </span>);
            editedCourseNames.push(' or ');
        })

        editedCourseNames.pop();

        if (this.props.courseIDs[0] === 'mat135') {
            // special case for calculus requirement since it doesn't fit the same pattern
            return (
                <p className='code'>
                    (<span id='mat135' className='courseName' onClick={this.openModal}>MAT135H</span>
                    <span> and </span>
                    <span id='mat136' className='courseName' onClick={this.openModal}>MAT136H</span>)
                    <span> or </span>
                    <span id='mat137' className='courseName' onClick={this.openModal}>MAT137Y</span>
                    <span> or </span>
                    <span id='mat157' className='courseName' onClick={this.openModal}>MAT157Y</span>
                 </p>
            )
        } else {
            return <p className='code'>{editedCourseNames}</p>;
        }
    }

    getIdName() {
        var idName = this.props.courseIDs[0].substring(0, 3);

        this.props.courseIDs.forEach((course) => {
            idName += course.substring(3, 6);
        });

        // math and stats courses need extra stuff appended to their IDs
        // (mainly to check if they are active or not through their local storage)
        if (this.props.courseIDs[0] === 'mat135') {
            idName += 'calc1';
        } else if (this.props.courseIDs[0] === 'mat221') {
            idName += 'lin1';
        } else if (this.props.courseIDs[0] === 'sta247') {
            idName += 'sta1';
        }

        return idName;
    }

    render() {
        var classes = '';

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
}


export class MultipleCourseCode extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            completed: false,
            completedTextBoxes: 0,
            infoOpened: false,
            textboxValues: this.createInitialValueArray()
        };

        this.toggleFullInfo = this.toggleFullInfo.bind(this);
        this.checkIfCompleted = this.checkIfCompleted.bind(this);
        this.isValidExtraCourse = this.isValidExtraCourse.bind(this);
        this.handleOnChange = this.handleOnChange.bind(this);
        this.countCompletedTextBoxes = this.countCompletedTextBoxes.bind(this);
    }

    componentWillMount() {
        this.setState({completedTextBoxes: this.state.completedTextBoxes +
                                           this.props.courses.length},
                      this.checkIfCompleted);
    }

    toggleFullInfo() {
        this.setState({infoOpened: !this.state.infoOpened});
    }

    checkIfCompleted() {
        this.setState({completed: this.state.completedTextBoxes === this.props.textBoxNumber});
    }

    createInitialValueArray() {
        var array = this.props.courses.slice();
        for (var i = this.props.courses.length; i < this.props.textBoxNumber; i++) {
            array.push('');
        }
        return array;
    }

    isValidExtraCourse(course) {
        var validCourseCodes = ['CSC', 'MAT', 'STA', 'ECE', 'BCB'];
        return validCourseCodes.indexOf(course.substring(0, 3)) > -1 &&
               course.length === 6;
    }

    handleOnChange(e) {
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
    }

    countCompletedTextBoxes() {
        return this.state.textboxValues.filter((v) => {
            return this.isValidExtraCourse(v);
        }).length;
    }

    render() {
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
                        {Array.apply(0, Array(this.props.textBoxNumber)).map((x, i) => {
                            if (this.isValidExtraCourse(this.state.textboxValues[i].substring(0, 6))) {
                                var className = 'valid_extra_course';
                            } else {
                                var className = 'not_valid_extra_course';
                            }

                            return (
                                <input type='text'
                                       key={i}
                                       id={i}
                                       className={className}
                                       value={this.state.textboxValues[i].substring(0, 6)}
                                       onChange={this.handleOnChange}
                                       disabled={this.props.textboxesDisabled} />
                            );
                        })}
                    </p>
                </div>
            </div>
        );
    }
}


export function CourseCategory(props) {
    return (
        <div>
            <h2>{props.yearName}</h2>
            {props.courses.map((courses) =>
                    <CourseCode id={courses[0]}
                                key={courses[0]}
                                courseIDs={courses}
                                openModal={props.openModal} />
            )}
        </div>
    );
}


export function CourseCategory2(props) {
    var yearName = props.yearName;
    var postType = props.otherInfo.postType;
    var textBoxes = props.otherInfo.textBoxes;
    var courseCategoryArrays = props.courseCategoryArrays;
    var changeCreditCount = props.changeCreditCount;
    var getInquiryCourse = props.getInquiryCourse;
    var hasInquiryCategory = props.otherInfo.hasInquiryCategory;

    return (
        <div className="col-md-4 col-sm-6">
            <div className="year_name">{yearName}</div>
            <div className="portfolio-thumb">
                    <ul className="year_course_list">
                    <li>
                        {props.courses.map((courses, i) =>
                            <CourseCode id={courses[0]}
                                key={i}
                                courseIDs={courses}
                                openModal={props.openModal} />
                        )}
                    </li>

                    <li>
                        {props.titles.map((title, i) => {
                        return <MultipleCourseCode courseID={postType + '_category_' + (i + 1)}
                                                    textBoxNumber={textBoxes[i][0]}
                                                    courses={courseCategoryArrays[i]}
                                                    textboxesDisabled={textBoxes[i][1]}
                                                    changeCourseCredit={changeCreditCount}
                                                    categoryName={title}
                                                    key={i} />
                    })}
                    </li>
                    <li>
                        {(() => {
                            if (hasInquiryCategory && yearName=='Later Years' ) {
                                return <InquiryCategory courseID={postType + '_inq'} course={getInquiryCourse()}
                                        categoryName='Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H,
                                        CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H, or PEY (0.5 FCEs)
                                        ** Note: Type "PEY" for Check my POSt to recognize it **' />
                            }
                        })()}
                    </li>
                </ul>
            </div>
        </div>
    );
}


export class InquiryCategory extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            completed: false,
            infoOpened: false,
            value: this.props.course
        };

        this.toggleFullInfo = this.toggleFullInfo.bind(this);
        this.handleOnChange = this.handleOnChange.bind(this);
    }

    componentWillMount() {
        this.setState({completed: this.props.course !== ''});
    }

    toggleFullInfo() {
        this.setState({infoOpened: !this.state.infoOpened});
    }

    handleOnChange(e) {
        var newValue = e.target.value;
        this.setState({value: newValue}, function () {
            if (this.state.value === 'PEY' || CSCinq.indexOf(this.state.value) >= 0) {
                this.setState({completed: true});
            } else {
                this.setState({completed : false});
            }
        });
    }

    render() {
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
}
