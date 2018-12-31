import React from 'react';
import ReactModal from 'react-modal';


class ModalContent extends React.Component {
  render() {
    return (
      <div>
        <Description course={this.props.course} />
      </div>
    );
  }
}


class Modal extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      courseId: '',
      course: [],
      sessions: [],
      modalIsOpen: false,
      courseTitle: '',
    };
    this.openModal = this.openModal.bind(this);
    this.closeModal = this.closeModal.bind(this);
  }

  openModal(newCourse) {
    if (newCourse === this.state.courseId) {
      this.setState({ modalIsOpen: true });
    } else {
      let formatted = formatCourseName(newCourse);
      getCourse(formatted[0])
        .then(course => {
          //This is getting the session times
          let sessions = course.fallSession.lectures
            .concat(course.springSession.lectures)
            .concat(course.yearSession.lectures);
            //Tutorials don't have a timeStr to print, so I've currently omitted them
          this.setState({
            course: course,
            sessions: sessions,
            modalIsOpen: true,
            courseId: newCourse,
            courseTitle: `${newCourse.toUpperCase()} ${course.title}`
          });
        })
    }
  }

  closeModal() {
    this.setState({ modalIsOpen: false });
  }

  render() {
    return (
      <ReactModal className='modal-class'
        overlayClassName='overlay'
        isOpen={this.state.modalIsOpen}
        onRequestClose={this.closeModal}
        ariaHideApp={false}
      >
        <div className='modal-header'>
          {this.state.courseTitle}
        </div>
        <div className='modal-body'>
          <Description
            course = {this.state.course}
            sessions = {this.state.sessions}
          />
        </div>
      </ReactModal>
    );
  }
}


//Use React component from search.js
class Description extends React.Component {
  render() {
    //We want to use the Timetable component, but that component needs to be independent before using it here
    return (
      <div>
        <p>{this.props.course.description}</p>
        <p><strong>Prerequisite: </strong>{this.props.course.prereqString}</p>
        <p><strong>Distribution Requirement Status: </strong>{this.props.course.distribution}</p>
        <p><strong>Breadth Requirement: </strong>{this.props.course.breadth}</p>
        <p><strong>Timetable: </strong></p>
        {this.props.sessions.map(function (lecture, i) {
          return <p key={i}>{lecture.code + lecture.session + '-' + lecture.section}</p>;
        })}
        {/*<Video urls={this.props.course.videoUrls} />*/}
      </div>
    );
  }
}


class Video extends React.Component {
  // TODO: put this back in.
  // static defaultProps = {
  //   urls: []
  // }

  render() {
    if (this.props.urls.length > 0) {
      return (
        <div id='course-video-div'>
          <video id='course-video' className='video-js vjs-default-skin' controls='' preload='auto'>
            {this.props.urls.map(function (url) {
              return <source src={url} type='video/mp4' key={url} />
            })}
          </video>
        </div>
      );
    } else {
      return <div></div>;
    }
  }
}


export { Modal };
