export var ModalContent = React.createClass({
    render: function() {
        return (
            <div>
                <Description course={this.props.course} />
            </div>
        );
    }
});

export var Modal = React.createClass({
    getInitialState: function () {
        return {
            courseId: '',
            course: [],
            sessions: []
        };
    },

    render: function () {
        if (this.state.courseId) {
            return (
                <div className='modal fade'>
                    <div className='modal-dialog'>
                        <div className='modal-content'>
                            <div className='modal-header'>
                                {getCourseTitle(this.state.courseId)}
                            </div>
                            <div className='modal-body'>
                                <Description course={formatCourseName(this.state.courseId)[0]}/>
                            </div>
                            <div className='modal-footer'>
                            </div>

                        </div>
                    </div>
                </div>
            );
        } else {
            return <div className='modal fade'></div>;
        }
    }
});

//Use React component from search.js
var Description = React.createClass({
    getInitialState: function() {
        return {
            course: [],
            sessions: []
        };
    },

    componentDidMount: function() {
        this.refresh();
    },

    componentWillUpdate: function (newProps, newState) {
        if (newProps.course !== this.props.course) {
            this.refresh(newProps.course);
        }
        var node = ReactDOM.findDOMNode(this)
        node.scrollIntoView();
    },

    // This loads the course json
    refresh: function(newCourse) {
        if (newCourse === undefined) {
            newCourse = this.props.course;
        }
        $.ajax({
            url: 'course',
            data: {name: newCourse},
            dataType: 'json',
            success: function(data) {
                if (this.isMounted()) {
                    this.setState({course: data});
                    //This is getting the session times
                    var sessions = data.fallSession.lectures
                                                   .concat(data.springSession.lectures)
                                                   .concat(data.yearSession.lectures)
                    //Tutorials don't have a timeStr to print, so I've currently ommitted them
                    this.setState({sessions: sessions});
                }
            }.bind(this),
            error: function(xhr, status, err) {
                console.error('course-info', status, err.toString());
            }.bind(this)
        });
    },

    render: function() {
        //We want to use the Timetable component, but that component needs to be independent before using it here
        return (
            <div>
                <p>{this.state.course.description}</p>
                <p><strong>Prerequisite: </strong>{this.state.course.prereqString}</p>
                <p><strong>Distribution Requirement Status: </strong>{this.state.course.distribution}</p>
                <p><strong>Breadth Requirement: </strong>{this.state.course.breadth}</p>
                <p><strong>Timetable: </strong></p>
                {this.state.sessions.map(function(lecture) {
                    return <p>{lecture.code + lecture.session + '-' + lecture.section + ': ' + lecture.timeStr}</p>;
                })}
                <Video urls={this.state.course.videoUrls}/>
            </div>
        );
    }
});

var Video = React.createClass({
    getDefaultProps: function() {
        return {
            urls: []
        }
    },

    render: function() {
        return (
            <div id='course-video-div'>
                <video id='course-video' className='video-js vjs-default-skin' controls='' preload='auto'>
                    {this.props.urls.map(function(url) {
                        return <source src={url} type='video/mp4'/>
                    })}
                </video>
            </div>
        );
    }
});
