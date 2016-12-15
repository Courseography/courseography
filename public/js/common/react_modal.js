import * as ReactModal from 'vendor/react-modal';

var ModalContent = React.createClass({
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
            sessions: [],
            modalIsOpen: false,
            courseTitle : '',
        };
    },

    openModal: function(newCourse) {
        if (newCourse == this.state.courseId) {
                this.setState({modalIsOpen: true});
        }else {
            var formatted = formatCourseName(newCourse);
            var that = this;
            this.setState({modalIsOpen: true, courseId: newCourse, courseTitle: getCourseTitle(newCourse, formatted)});

            $.ajax({
                    url: 'course',
                    data: {name: formatted[0]},
                    dataType: 'json',
                    success: function (data) {
                            //This is getting the session times
                            var sessions = data.fallSession.lectures
                                                           .concat(data.springSession.lectures)
                                                           .concat(data.yearSession.lectures)
                            //Tutorials don't have a timeStr to print, so I've currently omitted them
                            that.setState({course: data, sessions: sessions});
                    },
                    error: function (xhr, status, err) {
                        console.error('course-info', status, err.toString());
                    }
                });
        }

    },
    closeModal : function() {
        this.setState({modalIsOpen: false});
    },
    render: function () {
            return (
                <div>
                <ReactModal className='ModalClass'
                    overlayClassName='OverlayClass'
                    isOpen={this.state.modalIsOpen}
                    onRequestClose={this.closeModal}>
                    <div className='modal-header'>
                        {this.state.courseTitle}
                          </div>
                        <div className='modal-body'>
                            <Description 
                            course = {this.state.course}
                            sessions = {this.state.sessions} /> </div> 
                     <div className='modal-footer'> </div>    
                </ReactModal>
                </div> );
       }


});

//Use React component from search.js
var Description = React.createClass({
    render: function() {
        //We want to use the Timetable component, but that component needs to be independent before using it here
        return (
            <div>
                <p>{this.props.course.description}</p>
                <p><strong>Prerequisite: </strong>{this.props.course.prereqString}</p>
                <p><strong>Distribution Requirement Status: </strong>{this.props.course.distribution}</p>
                <p><strong>Breadth Requirement: </strong>{this.props.course.breadth}</p>
                <p><strong>Timetable: </strong></p>
                {this.props.sessions.map(function(lecture) {
                    return <p>{lecture.code + lecture.session + '-' + lecture.section + ': ' + lecture.timeStr}</p>;
                })}
                <Video urls={this.props.course.videoUrls}/>
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
