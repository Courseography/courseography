import React from 'react';
import ReactModal from 'react-modal';
import Leaflet from 'leaflet';
import {CircleMarker, Polygon, Polyline, Map, TileLayer, Marker, Popup, Tooltip, Circle } from 'react-leaflet';

class ModalContent extends React.Component {
  render() {
    return (
      <div>
        <Description course={this.props.course} />
      </div>
    );
  }
}


class CourseModal extends React.Component {
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
      getCourse(newCourse)
        .then(course => {
            //Tutorials don't have a timeStr to print, so I've currently omitted them
          this.setState({
            course: course,
            sessions: course.allMeetingTimes.sort((firstLec, secondLec) =>
              firstLec.meetData.session > secondLec.meetData.session ? 1 : -1),
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
          return <p key={i}>{lecture.meetData.code + lecture.meetData.session + '-' + lecture.meetData.section}</p>;
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


/**
 * Returns and formats all course codes in id.
 * @param {string} id The Node's ID.
 * @returns {string[]} All formatted course codes.
 * TODO: Change function name
 */
function formatCourseName(id) {
  var names;

  if (id === 'CSC200') {
      names = id + 'Y1';
  } else if (id === 'Calc1') {
      names = 'MAT135H1' + ' ' + 'MAT136H1' + ' ' + 'MAT137Y1' + ' ' +
              'MAT157Y1';
  } else if (id === 'Lin1') {
      names = 'MAT221H1' + ' ' + 'MAT223H1' + ' ' + 'MAT240H1';
  } else if (id === 'Sta1') {
      names = 'STA247H1' + ' ' + 'STA255H1';
  } else if (id === 'Sta2') {
      names = 'STA248H1' + ' ' + 'STA261H1';
  } else if (id.indexOf('H1', id.length - 2) !== -1) {
      names = id;
  } else {
      names = id + 'H1';
  }

  names = names.split(" ");
  return names;
}


class MapModal extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      modalIsOpen: false
    };
    this.openModal = this.openModal.bind(this);
    this.closeModal = this.closeModal.bind(this);
  }

  openModal() {
    this.setState({ modalIsOpen: true });
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
        <div className='modal-body'>
          <TestMap/>
        </div>
      </ReactModal>
    );
  }
}


// A Temporary component as a placeholder for the campus map
class TestMap extends React.Component {
  render() {
    const center = [43.65977015, -79.3972632658009];
    const polyline = [[43.65977015, -79.3972632658009], [43.6623705, -79.398659815008]]

    const multiPolyline = [
      [[43.66977015, -79.3972632658009], [43.6623705, -79.39859815008]],
      [[43.66987015, -79.398632658009], [43.6633705, -79.398659815008]],
    ]

    const polygon = [[43.66977015, -79.3972632658009], [43.6623705, -79.39859815008], [43.6623705, -79.40859815008]]

    const customMarker = new L.Icon({
      iconUrl: 'static/res/ico/map.png',
      shadowUrl: 'static/res/ico/shadow.png',
      iconAnchor: [5, 55],
      popupAnchor: [10, -44],
      iconSize: [25, 42],
      shadowSize: [50, 85],
      shadowAnchor: [2, 95],
    })

    return (
      <div id="campus-map">
        <Map
          center={[43.65977015, -79.3972632658009]}
          zoom={16}
          maxZoom={40}
          attributionControl={true}
          zoomControl={true}
          doubleClickZoom={true}
          scrollWheelZoom={true}
          dragging={true}
          animate={true}
        >
          <TileLayer url="http://{s}.tile.osm.org/{z}/{x}/{y}.png" />
          <Marker icon={customMarker} position={[43.65977015, -79.3972632658009]}>
            <Popup><p>ACT348</p> Date and Time</Popup>
          </Marker>
          <Marker icon={customMarker} position={[43.6623705, -79.398659815008]}>
            <Popup>popup 2.</Popup>
          </Marker>
          <Circle
          center={[43.65977015, -79.3972632658009]}
          fillColor="blue"
          radius={200}>
          <Tooltip>hello</Tooltip>
          </Circle>

          <Circle center={center} fillColor="blue" radius={200} />
          <CircleMarker center={[51.51, -0.12]} color="red" radius={20}>
            <Popup>Popup in CircleMarker</Popup>
          </CircleMarker>
          <Polyline color="lime" positions={polyline} />
          <Polyline color="red" positions={multiPolyline} />
          <Polygon color="purple" positions={polygon} />
        </Map>
      </div>
    );
  }
}

export { CourseModal, MapModal };
