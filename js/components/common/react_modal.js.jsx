import React, { createRef } from 'react';
import ReactModal from 'react-modal';
import Leaflet from 'leaflet';
import { Circle, CircleMarker, FeatureGroup, Map, Marker, Polygon, Polyline, Popup, TileLayer, Tooltip } from 'react-leaflet';
import L from 'leaflet'
import { getCourse } from '../common/utils';

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
 * Modal containing a map with markers corresponding to the locations of the courses currently on the
 * time table
 */
class MapModal extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      modalIsOpen: false,
      selectedLecTimeframes: []
    };
    this.openModal = this.openModal.bind(this);
    this.closeModal = this.closeModal.bind(this);
    this.groupLecturesByBuilding = this.groupLecturesByBuilding.bind(this);
    this.selectLecTimeframe = this.selectLecTimeframe.bind(this);
    this.addDayTimeframes = this.addDayTimeframes.bind(this);
    this.removeDayTimeframes = this.removeDayTimeframes.bind(this);
    this.isDaySelected = this.isDaySelected.bind(this);
    this.selectDayTimeframes = this.selectDayTimeframes.bind(this);
  }

  openModal() {
    this.setState({ modalIsOpen: true });
  }

  closeModal() {
    this.setState({ modalIsOpen: false });
  }

  // Group each lecture time by the building they occur in. If the lecture has 2 rooms in different
  // buildings, it will appear under both of the buildings. Lectures for each building are grouped by
  // their course codes
  groupLecturesByBuilding(lecsByBuilding, lecture, roomNum) {
    const foundBuilding = lecsByBuilding.find(b => b.buildingName === lecture[roomNum].bName);

    const timeframeData = {
      courseCode: lecture.courseCode,
      session: lecture.session,
      startTime: lecture.startTime,
      endTime: lecture.endTime,
      room: lecture[roomNum].room
    }

    const dayData = {
      dayString: lecture.dayString,
      dayOrder: lecture.day,
      timeframes: [timeframeData]
    };

    // if the building is not in the lecsByBuilding yet, add it into the array
    if (!foundBuilding) {
      lecsByBuilding.push({
        buildingName: lecture[roomNum].bName,
        buildingCode: lecture[roomNum].bCode,
        address: lecture[roomNum].address,
        lat: lecture[roomNum].lat,
        lng: lecture[roomNum].lng,
        postalCode: lecture[roomNum].postalCode,
        days: [dayData]
      });
    }
    // the building is already in lecsByBuilding, so add the timeframe to the course if the course already
    // exists. Otherwise, add the course with the timeframe
    else {
      const foundDay = foundBuilding.days.find(c => c.dayOrder === lecture.day);

      if (!foundDay) {
        foundBuilding.days.push(dayData);
      }
      else {
        foundDay.timeframes.push(timeframeData);
      }
    }
  }

  // Add lecture to selectedLecTimeframes if lecture does not already exist in selectedLecTimeframes.
  // Otherwise, remove it from selectedLecTimeframes
  selectLecTimeframe(lecture) {
    let updatedLecTimeframes = this.state.selectedLecTimeframes.slice();

    const lecTimeframeInd = this.findSelectedLecTimeframe(lecture, updatedLecTimeframes);

    if (lecTimeframeInd == -1) {
      updatedLecTimeframes.push(lecture);
    }
    else {
      updatedLecTimeframes.splice(lecTimeframeInd, 1);
    }

    this.setState({selectedLecTimeframes: updatedLecTimeframes});
  }

  // Add each element of lectures that does not already exist in selectedLecTimeframes to selectedLecTimeframes
  addDayTimeframes(lectures) {
    let updatedLecTimeframes = this.state.selectedLecTimeframes.slice();

    for (let lec of lectures) {
      if (this.findSelectedLecTimeframe(lec, updatedLecTimeframes) == -1) {
        updatedLecTimeframes.push(lec);
      }
    }

    this.setState({selectedLecTimeframes: updatedLecTimeframes});
  }

  // Remove each element of lectures from selectedLecTimeframes
  removeDayTimeframes(lectures) {
    let updatedLecTimeframes = this.state.selectedLecTimeframes.slice();

    for (let lec of lectures) {
      const lecTimeframeInd = this.findSelectedLecTimeframe(lec, updatedLecTimeframes);

      if (lecTimeframeInd != -1) {
        updatedLecTimeframes.splice(lecTimeframeInd, 1);
      }
    }

    this.setState({selectedLecTimeframes: updatedLecTimeframes});
  }

  // If all elements of lectures are in selectedLecTimeframes, remove all elements of lectures
  // from selectedLecTimeframes. Otherwise, add all elements of lectures to selectedLecTimeframes
  selectDayTimeframes(lectures) {
    if (this.isDaySelected(lectures)) {
      this.removeDayTimeframes(lectures);
    }
    else {
      this.addDayTimeframes(lectures);
    }
  }

  // Returns True if all elements of lectures are in selectedLecTimeframes
  isDaySelected(lectures) {
    const updatedLecTimeframes = this.state.selectedLecTimeframes.slice();
    return lectures.every(lec => this.findSelectedLecTimeframe(lec, updatedLecTimeframes) != -1);
  }

  // Returns the index of lecture in selectedLecTimeframes if it exists. Otherwise, return -1
  findSelectedLecTimeframe(lecture, selectedLectures) {
    return selectedLectures.findIndex(lec =>
      lec.courseCode === lecture.courseCode &&
      lec.session === lecture.session &&
      lec.day == lecture.day &&
      lec.startTime == lecture.startTime &&
      lec.endTime == lecture.endTime
    );
  }

  render() {
    let lecturesByBuilding = [];

    let lecturesByDay = {};

    this.props.lectures.forEach(lecture => {
      if (lecture.fstRoom) {
        this.groupLecturesByBuilding(lecturesByBuilding, lecture, 'fstRoom');
      }
      if (lecture.secRoom) {
        this.groupLecturesByBuilding(lecturesByBuilding, lecture, 'secRoom');
      }

      if (lecturesByDay[lecture.dayString]) {
        lecturesByDay[lecture.dayString].push(lecture);
      }
      else {
        lecturesByDay[lecture.dayString] = [lecture];
      }
    });

    return (
      <ReactModal className='modal-class'
        overlayClassName='overlay'
        isOpen={this.state.modalIsOpen}
        onRequestClose={this.closeModal}
        ariaHideApp={false}
      >
        <div className='modal-body'>
          <CampusMap
            lecturesByBuilding={lecturesByBuilding}
            selectedLecTimeframes={this.state.selectedLecTimeframes}
          />
          <MapSidebar
            lecturesByDay={lecturesByDay}
            selectLecTimeframe={this.selectLecTimeframe}
            selectDayTimeframes={this.selectDayTimeframes}
            isDaySelected={this.isDaySelected}
            findSelectedLecTimeframe={this.findSelectedLecTimeframe}
            selectedLecTimeframes={this.state.selectedLecTimeframes}
          />
        </div>
      </ReactModal>
    );
  }
}


/**
 * The sidebar for the Map modal
 */
class MapSidebar extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    let dayLectures = [];

    const days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"];

    for (let day of days) {
      if (this.props.lecturesByDay[day]) {
        dayLectures.push(
          <DayBox
            key={day}
            day={day}
            dayLectures={this.props.lecturesByDay[day]}
            selectLecTimeframe={this.props.selectLecTimeframe}
            selectDayTimeframes={this.props.selectDayTimeframes}
            isDaySelected={this.props.isDaySelected}
            findSelectedLecTimeframe={this.props.findSelectedLecTimeframe}
            selectedLecTimeframes={this.props.selectedLecTimeframes}
          />
        );
      }
    }

    return (
      <div id="map-sidebar">
        {dayLectures}
      </div>
    );
  }
}


/**
 * An expandable weekday box with its corresponding lectures
 */
class DayBox extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      expanded: false
    }

    this.toggleExpand = this.toggleExpand.bind(this);
    this.getRoomStr = this.getRoomStr.bind(this);
  }

  toggleExpand() {
    this.setState({expanded: !this.state.expanded})
  }

  getRoomStr(lec, room, roomNum) {
    return "Room" + (lec.fstRoom && lec.secRoom ? " " + roomNum : "") + ": " + room.room + ", " + room.bName + " (" + room.bCode + ")";
  }

  render() {
    const isSelected = this.props.isDaySelected(this.props.dayLectures);

    const arrowColour = isSelected ? "lightpurple" : "darkpurple";

    return (
      <div>
        <div className={"sidebar-day " +
          (this.state.expanded ? "accordian-expanded" : "accordian-collapsed") +
          (isSelected ? " selected-day" : " unselected-day")}>

          <div id="map-day-title">
            <h3
              onClick={ () => this.props.selectDayTimeframes(this.props.dayLectures) }
              style={{'display': 'inline-block'}}
            > {this.props.day} </h3>
          </div>

          <img src={"static/res/ico/" + (this.state.expanded ? "doubleup-" : "doubledown-") + (isSelected ? "lightpurple" : "darkpurple") + ".png"}
            id="expand-icon"
            onClick={this.toggleExpand}/>
        </div>

        {this.state.expanded &&
          <ul id="map-day-list">
            {this.props.dayLectures.map(lec => {
              let roomStr = "";
              let roomNum = 0;
              if (lec.fstRoom) {
                roomNum += 1;
                roomStr += this.getRoomStr(lec, lec.fstRoom, roomNum);
              }
              if (lec.secRoom) {
                roomNum += 1;
                roomStr += (lec.fstRoom ? "\n" : "") + this.getRoomStr(lec, lec.secRoom, roomNum);
              }

              return (
                <li key={lec.courseCode + "-" + lec.startTime + "-" + lec.endTime}
                  className={ "map-day-lec " + (this.props.findSelectedLecTimeframe(lec, this.props.selectedLecTimeframes) === -1 ? "unselected-day-lec" : "selected-day-lec") }
                  onClick={ () => this.props.selectLecTimeframe(lec) }>
                  <h4> {lec.courseCode} </h4>
                  <h5> {convertTimeToString(lec.startTime) + "-" + convertTimeToString(lec.endTime)} </h5>
                  <h5> {roomStr} </h5>
                </li>
              )}
            )}
          </ul>
        }
      </div>
    );
  }
}


/**
 * The map of the St.George campus used in the map modal
 */
class CampusMap extends React.Component {
  constructor(props) {
    super(props);

    this.zoomToFit = this.zoomToFit.bind(this);
    this.mapRef = createRef();
    this.groupRef = createRef();
  }

  zoomToFit() {
    const map = this.mapRef.current.leafletElement;
    const mapGroup = this.groupRef.current.leafletElement;

    // markerCount is the number of markers on the map.
    // Only adjust the map bounds if there are multiple markers because the zoom will
    // be strange for 1 marker. Note that the map will be centered on the single marker
    const markerCount = mapGroup.getLayers().length

    if (markerCount > 1) {
      map.fitBounds(mapGroup.getBounds(), {padding: [30, 30]});
    }
  }

  render() {
    const blueMarker = new L.Icon({
      iconUrl: 'static/res/ico/blue-marker.png',
      shadowUrl: 'static/res/ico/shadow.png',
      iconAnchor: [5, 35],
      popupAnchor: [13, -35],
      iconSize: [34, 35],
      shadowSize: [50, 85],
      shadowAnchor: [2, 90]
    })

    const redMarker = new L.Icon({
      iconUrl: 'static/res/ico/red-marker.png',
      shadowUrl: 'static/res/ico/shadow.png',
      iconAnchor: [5, 35],
      popupAnchor: [13, -35],
      iconSize: [34, 35],
      shadowSize: [50, 85],
      shadowAnchor: [2, 90]
    })

    const locationMarkers = this.props.lecturesByBuilding.map(building => {
      const description = building.days
        .sort((dayA, dayB) => {return dayA.dayOrder - dayB.dayOrder})
        .map(day => {
          const dayTimes = day.timeframes
            .sort((timeA, timeB) => {return timeA.startTime - timeB.startTime})
            .map(time =>
              <li key={time.courseCode + "-" + time.startTime + "-" + time.endTime}>
                {convertTimeToString(time.startTime) + "-" + convertTimeToString(time.endTime) + ", "+ time.room + ": " + time.courseCode}
              </li>
            );

          return (
            <div key={day.dayString}>
              <p>
                {day.dayString}
              </p>
              {dayTimes}
            </div>
          );
        });

      let colouredMarker;
      const buildingInd = this.props.selectedLecTimeframes.findIndex(lec =>
        (lec.fstRoom && lec.fstRoom.bCode === building.buildingCode) ||
        (lec.secRoom && lec.secRoom.bCode === building.buildingCode)
      );

      colouredMarker = (buildingInd == -1) ? blueMarker : redMarker

      return (
        <Marker key={building.buildingName} icon={colouredMarker} position={[building.lat, building.lng]}>
          <Popup key={building.postalCode}>
            <p>
              <b> {building.buildingName + " | " + building.buildingCode} </b>
            </p>
            <p>
              <em>{building.address}</em>
            </p>
            {description}
          </Popup>
        </Marker>
      );
    });

    let mapBounds = L.latLngBounds();

    // If there are selected lectures, center the map based on the locations of those lectures, otherwise, center the map
    // on Bahen
    let center = [43.6596426,-79.3976676];

    if (this.props.lecturesByBuilding.length) {

      this.props.lecturesByBuilding.forEach(building =>
        mapBounds.extend(L.latLng(building.lat, building.lng)));

      center = mapBounds.getCenter();
    }

    return (
      <div ref="map" id="campus-map" className="col-md-8 col-xs-6">
        <Map
          center={center}
          zoom={16}
          maxZoom={40}
          attributionControl={true}
          zoomControl={true}
          doubleClickZoom={true}
          scrollWheelZoom={true}
          dragging={true}
          animate={true}
          ref={this.mapRef}
        >
          <TileLayer url="http://{s}.tile.osm.org/{z}/{x}/{y}.png"/>
          <FeatureGroup ref={this.groupRef}>
            {locationMarkers}
          </FeatureGroup>
        </Map>
        <button id="zoom-button" onClick={this.zoomToFit}>Zoom to Fit</button>
      </div>
    );
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
  } else if (id === 'Alg1') {
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


// Helper function to get the string representation of the time
function convertTimeToString(time) {
  const meridiem = time < 12 ? "AM" : "PM";

  if (time % 1 === 0) {
    return (time === 12 ? 12 : time % 12) + ':00' + meridiem;
  }
  return (time === 12.5 ? 12 : Math.floor(time)) % 12 + ':30' + meridiem;
}

export { CourseModal, MapModal };
