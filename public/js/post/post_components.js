import {CourseCategory, MultipleCourseCode, InquiryCategory} from 'es6!post/course_components';

/**
 * Returns whether course is a specialist course or not
 * @param {string} course Name of course
 * @return {boolean} True if course is a specialist, False otherwise
 */
function notSpecialistCourse(course) {
    'use strict';

    return specialistCourses.indexOf(course) === -1;
}

var CheckMyPost = React.createClass({
    changeActiveTab: function(newTab) {
        var activeTab = newTab;
        var tabs = ['spePost', 'majPost', 'minPost'];
        var me = this;

        tabs.forEach(function(tab) {
            if (tab === activeTab) {
                me.refs[tab].changeTabView(true);
            } else {
                me.refs[tab].changeTabView(false);
            }
        });
    },

    render: function() {
        return (
            <div id='check_my_post'>
                <PostNav ref='postNav' updateTab={this.changeActiveTab}/>
                <SpecialistPost ref='spePost'/>
                <MajorPost ref='majPost' />
                <MinorPost ref='minPost' />
            </div>
        );
    }
});


var PostNav = React.createClass({
    getInitialState: function() {
        return {
            visible: this.getActiveTab()
        }
    },

    getActiveTab: function() {
        if (getCookie('minor') === 'active') {
           return 'min';
        } else if (getCookie('major') === 'active') {
            return 'maj';
        } else {
            return 'spe';
        }
    },

    changeActiveTab: function(e) {
        var newVisible = e.target.id.substring(0, 3);
        this.setState({visible: newVisible}, this.props.updateTab(newVisible + 'Post'));
    },

    getNavClass: function(type) {
        return this.state.visible === type ? 'nav_selected' : 'nav_not_selected';
    },

    render: function() {
        return (
            <nav id='posts'> 
                <ul>
                    <li id='specialist' className={this.getNavClass('spe')}>
                        <a id='spec_link' onClick={this.changeActiveTab}> Specialist </a>
                        <div id='spec_creds'> (0/12.0) </div>
                    </li>
                    <li id='major' className={this.getNavClass('maj')}>
                        <a id='maj_link' onClick={this.changeActiveTab}> Major </a>
                        <div id='maj_creds'> (0/8.0) </div>
                    </li>
                    <li id='minor' className={this.getNavClass('min')}>
                        <a id='min_link' onClick={this.changeActiveTab}> Minor </a>
                        <div id='min_creds'> (0/4.0) </div>
                    </li>
                </ul>
            </nav>
        );
    }

});


var Post = React.createClass({
    getInitialState: function() {
        return {
            selected: false,
            activeCourses: this.updateActiveCourses(),
            creditCount: 0
        }
    },

    updateActiveCourses: function() {
        var activeCourses = [];

        return allCourses.concat(math).filter(function (course) {
            var status = getCookie(course.toLowerCase());
            return status === 'active' || status === 'overridden';
        });
    },

    componentWillMount: function() {
        this.setState({selected: getCookie(this.props.postType) === 'active'});
        this.calculateCreditCount();
    },

    componentWillReceiveProps: function(newProps) {
        this.setState({selected: newProps.isSelected});
    },

    isInquiryCourse: function(course) {
        return CSCinq.indexOf(course) > 0;
    },

    getCourses: function () {
        var courseChecks = this.props.courseChecks;
        var courseArrays = [];

        // initialize inner arrays
        for (var i = 0; i < courseChecks.length; i++) {
            courseArrays.push([]);
        }

        this.state.activeCourses.forEach(function (course) {
            for (var i = 0; i < courseChecks.length; i++) {
                if (courseChecks[i](course, courseArrays[i])) {
                    courseArrays[i].push(course);
                    break;
                }
            }
        });

        return courseArrays;
    },

    getInquiryCourse: function () {
        var inquiryCourses = this.state.activeCourses.filter(this.isInquiryCourse);
        return inquiryCourses.length === 0 ? '' : inquiryCourses[0];
    },

    changeCreditCount: function (value) {
        this.setState({creditCount: this.state.creditCount + value});
    },

    calculateCreditCount: function() {
        var count = 0;

        this.state.activeCourses.forEach(function (course) {
            var courseID = course.toLowerCase()
            if (getCookie(courseID) === 'active' || getCookie(courseID) === 'overridden') {
                if (course === 'MAT135136137157Calc1') {
                    count += 1;
                } else {
                    count += 0.5;
                }
            }
        });

        this.changeCreditCount(count);
    },

    render: function() {

        if (this.state.selected) {
            var classes = 'post_selected';
        } else {
            var classes = 'post_not_selected';
        }

        var courseCategoryArrays = this.getCourses();
        var me = this;

        return (
            <div id={'post_' + this.props.postType} className={classes} >
                <CourseCategory yearName='First Year' courses={this.props.firstYearCourses} />
                <CourseCategory yearName='Second Year' courses={this.props.secondYearCourses} />
                <CourseCategory yearName='Later Years' courses={this.props.laterYearCourses} />
                {this.props.categoryTitles.map(function (title, i) {
                    return <MultipleCourseCode courseID={me.props.postType + '_category_' + (i + 1)} 
                                               textBoxNumber={me.props.textBoxNumbers[i]} 
                                               courses={courseCategoryArrays[i]} 
                                               textboxesDisabled={me.props.disabledTextboxes[i]} 
                                               changeCourseCredit={me.changeCreditCount} 
                                               categoryName={title}
                                               key={i} /> 
                })}
                {(() => {
                    if (this.props.hasInquiryCategory) {
                        return <InquiryCategory courseID={this.props.postType + '_inq'} course={this.getInquiryCourse()} 
                                categoryName='Any from this list: CSC301H, CSC318H, CSC404H, CSC411H, CSC418H, CSC420H, 
                                CSC428H, CSC454H, CSC485H, CSC490H, CSC491H, CSC494H, or PEY (0.5 FCEs) 
                                ** Note: Type "PEY" for Check my POSt to recognize it **' />
                    }
                })()}
                <h2>Notes</h2>
                <ul id='notes'>
                    {this.props.notes.map(function (note, i) {
                        return <li key={i}>{note}</li>
                    })}
                </ul>
            </div>
        );
    }
});


var SpecialistPost = React.createClass({ 
    getInitialState: function() {
        return {
            selected: false
        }
    },

    changeTabView: function(isSelected) {
        this.setState({selected: isSelected});
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

    render: function() {

        var categoryTitles = ['Any 400-level CSC course, BCB410H, BCB420H, BCB430Y, ECE489H (1.5 FCEs)', 
                              'Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.5 FCEs)',
                              'Any of the following: 300+ level CSC course; MAT: 235/237/257, any 300+ \
                               except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H; \
                               BCB: 410H/420H/430Y (2.0 FCEs)'];
        var notes = ['No more than 1.0 FCE from CSC490H1, CSC491H1, CSC494H1, CSC495H1, BCB430Y1 may be used \
                     to fulfill program requirements'];

        var firstYearCourses = [['csc108'], ['csc148'], ['csc165', 'csc240'], ['mat135', 'mat136', 'mat137', 'mat157']];
        var secondYearCourses = [['csc207'], ['csc209'], ['csc236', 'csc240'], ['csc258'], ['csc263', 'csc265'], ['mat221', 'mat223', 'mat240'], 
                                ['sta247', 'sta255', 'sta257']];
        var laterYearCourses = [['csc369'], ['csc373']];

        return (
            <Post postType='specialist' 
                  firstYearCourses={firstYearCourses} 
                  secondYearCourses={secondYearCourses} 
                  laterYearCourses={laterYearCourses} 
                  textBoxNumbers={[3, 3, 4]} 
                  disabledTextboxes={[true, true, false]}
                  courseChecks={[this.isLevel400, this.isLevel300, this.isLevelExtra]} 
                  categoryTitles={categoryTitles} 
                  notes={notes}
                  hasInquiryCategory={true}
                  isSelected={this.state.selected} />
        );
    }
});


var MajorPost = React.createClass({
    getInitialState: function() {
        return {
            selected: false
        }
    },

    changeTabView: function(isSelected) {
        this.setState({selected: isSelected});
    }, 

    isLevel400: function (course, level400Array) {
        return course.substring(3, 4) === '4' && level400Array.length < 1;
    },

    isLevel300: function (course, level300Array) {
        return course.substring(3, 4) >= '3' && level300Array.length < 2;
    },

    isLevelExtra: function (course, levelExtraArray) {
        return course.substring(3, 4) >= '3' && levelExtraArray.length < 3;
    },

    render: function() {

        var categoryTitles = ['Any 400-level CSC course, BCB410H, BCB420H, BCB430Y (0.5 FCEs)', 
                              'Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.0 FCEs)',
                              'Any of the following: 200+ level CSC course; MAT: 221/223/240, 235/237/257, any 300+ \
                               except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H; BCB: 410H/420H/430Y \
                              (1.5 FCEs, with at least 0.5 FCEs in the 300+ level)'];
        var notes = ['No more than 1.0 FCE from CSC490H1, CSC491H1, CSC494H1, CSC495H1, BCB430Y1 may be used \
                     to fulfill program requirements'];

        var firstYearCourses = [['csc108'], ['csc148'], ['csc165', 'csc240'], ['mat135', 'mat136', 'mat137', 'mat157']];
        var secondYearCourses = [['csc207'], ['csc236', 'csc240'], ['csc258'], ['csc263', 'csc265'], 
                                ['sta247', 'sta255', 'sta257']];
        var laterYearCourses = [];

        return (
            <Post postType='major' 
                  firstYearCourses={firstYearCourses} 
                  secondYearCourses={secondYearCourses} 
                  laterYearCourses={laterYearCourses}
                  textBoxNumbers={[1, 2, 3]} 
                  disabledTextboxes={[true, true, false]}
                  courseChecks={[this.isLevel400, this.isLevel300, this.isLevelExtra]} 
                  categoryTitles={categoryTitles}
                  notes={notes}
                  hasInquiryCategory={true} 
                  isSelected={this.state.selected} /> 
        );
    }
});


var MinorPost = React.createClass({
    getInitialState: function() {
        return {
            selected: false
        }
    },

    changeTabView: function(isSelected) {
        this.setState({selected: isSelected});
    }, 

    isLevelExtra: function (course, levelExtraArray) {
        var nonValidCourses = ['CSC207', 'CSC236240'];
        return course.substring(3, 4) >= '2' && nonValidCourses.indexOf(course) < 0 && levelExtraArray.length < 3;
    },

    render: function() {

        var categoryTitles = ['200+ CSC courses (1.5 FCEs, with at least 1.0 FCE in the 300+ levels)'];
        var notes = ['You may take no more than three 300+ CSC/ECE courses'];

        var firstYearCourses = [['csc108'], ['csc148'], ['csc165', 'csc240']];
        var secondYearCourses = [['csc207'], ['csc236', 'csc240']];
        var laterYearCourses = [];

        return (
            <Post postType='minor' 
                  firstYearCourses={firstYearCourses} 
                  secondYearCourses={secondYearCourses} 
                  laterYearCourses={laterYearCourses}
                  textBoxNumbers={[3]} 
                  disabledTextboxes={[false]}
                  courseChecks={[this.isLevelExtra]} 
                  categoryTitles={categoryTitles}
                  notes={notes}
                  hasInquiryCategory={false} 
                  isSelected={this.state.selected} /> 
        );
    }
});

export default {SpecialistPost: SpecialistPost, MajorPost: MajorPost, MinorPost: MinorPost, 
                CheckMyPost: CheckMyPost};