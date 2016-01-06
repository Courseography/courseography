var SpecialistPost = React.createClass({
    getInitialState: function() {
        return {
            selected: true,
            activeCourses: this.updateActiveCourses(),
            creditCount: 0
        }
    },

    componentWillMount: function() {
        this.setState({selected: getCookie('specialist') === 'active'});
        this.calculateCreditCount();
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

    isInquiryCourse: function(course) {
        return CSCinq.indexOf(course) > 0;
    },

    getCourses: function () {
        var courseChecks = [this.isLevel400, this.isLevel300, this.isLevelExtra];
        var courseArrays = [];

        // initialize inner arrays
        for (var i = 0; i < courseChecks.length; i++) {
            courseArrays[i] = [];
        }

        this.state.activeCourses.map(function (course) {
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
        return inquiryCourses == [] ? '' : inquiryCourses[0];
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

        var firstYearCourses = [['csc108'], ['csc148'], ['csc165', 'csc240'], ['mat135', 'mat136', 'mat137', 'mat157']];
        var secondYearCourses = [['csc207'], ['csc209'], ['csc236', 'csc240'], ['csc258'], ['csc263', 'csc265'], ['mat221', 'mat223', 'mat240'], 
                                ['sta247', 'sta255', 'sta257']];
        var laterYearCourses = [['csc369'], ['csc373']];

        var courseCategoryArrays = this.getCourses();

        return (
            <div id="specialist_window">
                <CourseCategory yearName='First Year' courses={firstYearCourses} />
                <CourseCategory yearName='Second Year' courses={secondYearCourses} />
                <CourseCategory yearName='Later Years' courses={laterYearCourses} />
                <MultipleCourseCode courseID='spec_400' textBoxNumber={3} courses={courseCategoryArrays[0]} textboxesDisabled={true} changeCourseCredit={this.changeCreditCount}
                    categoryName='Any 400-level CSC course, BCB410H, BCB420H, BCB430Y, ECE489H (1.5 FCEs)' />
                <MultipleCourseCode courseID='spec_300' textBoxNumber={3} courses={courseCategoryArrays[1]} textboxesDisabled={true} changeCourseCredit={this.changeCreditCount}
                    categoryName='Any 300+ level CSC course, BCB410H, BCB420H, BCB430Y, ECE385H, ECE489H (1.5 FCEs)' />
                <MultipleCourseCode courseID="spec_extra" textBoxNumber={4} courses={courseCategoryArrays[2]} textboxesDisabled={false} changeCourseCredit={this.changeCreditCount}
                    categoryName='Any of the following: 300+ level CSC course; MAT: 235/237/257, any 300+ 
                                  except for 329, 390, & 391; STA: 248, 261, any 300+; ECE: 385H/489H; 
                                  BCB: 410H/420H/430Y (2.0 FCEs)' />  
                <InquiryCategory courseID='spec_inq' course={this.getInquiryCourse()} 
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
