import React from 'react';
import ReactDOM from 'react-dom';
import ReactModal from 'react-modal';

ReactModal.setAppElement('#nav-export')

export class ExportModal extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            data: '',
            otherSession: 'Spring'
        };
        this.getImage = this.getImage.bind(this);
        this.getGraphImage = this.getGraphImage.bind(this);
        this.getGridImage = this.getGridImage.bind(this);
        this.openModal = this.openModal.bind(this);
        this.closeModal = this.closeModal.bind(this);
        this.toggleSession = this.toggleSession.bind(this);
    }

    getImage() {
        if (this.props.context === 'graph') {
            this.getGraphImage();
        } else {
            this.getGridImage(this.props.session);
        }
    }

    getGraphImage() {
        var necessaryLS = new Object();
        for (const elem in localStorage) {
            if (!localStorage.hasOwnProperty(elem)) {
                continue;
            }

            if (elem.substring(0,3).match(/^[a-zA-Z]+$/) && elem.substring(3,6).match(/^\d+$/)) {
                if (document.getElementById(elem)) {
                    necessaryLS[elem] = localStorage.getItem(elem);
                }
            } else {
                necessaryLS[elem] = localStorage.getItem(elem);
            }
        }

        var JsonLocalStorageObj = JSON.stringify(necessaryLS);
        $.ajax({
            url: 'image',
            data: {JsonLocalStorageObj: JsonLocalStorageObj},
            success: function (data) {
                this.setState({data: "data:image/png;base64," + data});
            }.bind(this),
            error: function () {
                throw 'No image generated';
            }
        });
    }

    getGridImage(session) {
        var formattedSession = session.charAt(0).toUpperCase() + session.slice(1);
        let allCourses = JSON.parse(localStorage.getItem('selectedLectures'));
        let courseData = allCourses.map((data) => `${data.courseCode.split(' ')[0]}-${data.lectureCode}-${data.session}`);
        $.ajax({
            url: 'timetable-image',
            data: {session: formattedSession, courses: courseData.join('_')},
            success: function (data) {
                this.setState({data: "data:image/png;base64," + data, otherSession: formattedSession === 'Fall' ? 'Spring' : 'Fall'});
            }.bind(this),
            error: function () {
                throw 'No image generated';
            }
        });
    }

    openModal() {
        this.setState({modalIsOpen: true}, this.getImage);
    }

    closeModal() {
        this.setState({modalIsOpen: false});
    }

    toggleSession() {
        this.getGridImage(this.state.otherSession);
    }

    render() {
        if (this.props.context === 'graph') {
            return (
                <ReactModal
                    className='modal-class'
                    overlayClassName='overlay'
                    isOpen={this.state.modalIsOpen}
                    onRequestClose={this.closeModal}>
                    <GraphImage data={this.state.data}/>
                </ReactModal>
            );
        } else {
            return (
                <ReactModal
                    className='modal-class'
                    overlayClassName='overlay'
                    isOpen={this.state.modalIsOpen}
                    onRequestClose={this.closeModal}>
                    <GridImage
                        data={this.state.data}
                        toggleSession={this.toggleSession} />
                </ReactModal>
            );
        }
    }
}


function getCalendar() {
    let allCourses = JSON.parse(localStorage.getItem('selectedLectures'));
    let courseData = allCourses.map((data) => `${data.courseCode.split(' ')[0]}-${data.lectureCode}-${data.session}`);
    $.ajax({
        type: "post",
        url: "calendar",
        data: {courses: courseData.join('_')},
        success: function (data) {
            var dataURI = "data:text/calendar;charset=utf8," + escape(data)
            var downloadLink = document.createElement("a");
            downloadLink.href = dataURI;
            downloadLink.download = "timetable.ics";
            document.body.appendChild(downloadLink);
            downloadLink.click();
            document.body.removeChild(downloadLink);

        },
        error: function () {
            throw 'No calendar avaiable';
        }
    });
}


function getPDF() {
    var necessaryLS = new Object();
    for (const elem in localStorage) {
        if (!localStorage.hasOwnProperty(elem)) {
            continue;
        }

        if (elem.substring(0,3).match(/^[a-zA-Z]+$/) && elem.substring(3,6).match(/^\d+$/)) {
            if (document.getElementById(elem)) {
                necessaryLS[elem] = localStorage.getItem(elem);
            }
        } else {
            necessaryLS[elem] = localStorage.getItem(elem);
        }
    }

    let allCourses = JSON.parse(localStorage.getItem('selectedLectures'));
    let courseData = allCourses.map((data) => `${data.courseCode.split(' ')[0]}-${data.lectureCode}-${data.session}`);

    $.ajax({
        url: "timetable-pdf",
        data: {courses: courseData.join('_'), JsonLocalStorageObj: JSON.stringify(necessaryLS)},
        success: function (data) {
            var dataURI = "data:application/pdf;base64," + data;
            var downloadLink = document.createElement("a");
            downloadLink.href = dataURI;
            downloadLink.download = "timetable.pdf";
            document.body.appendChild(downloadLink);
            downloadLink.click();
            document.body.removeChild(downloadLink);
        },
        error: function () {
            throw 'No pdf generated';
        }
    });
}


function GraphImage(props) {
    return (
        <div>
        <div className='modal-header'>
            Export
        </div>
        <div className='modal-body'>
            <a onClick={getPDF} href="#">Download PDF</a>
            <div>
            {props.data && <img id="post-image" src={props.data}/>}
            </div>
        </div>
        </div>
    );
}


function GridImage(props) {
    return (
        <div>
        <div className='modal-header'>
            Export
        </div>
        <div className='modal-body'>
            <a onClick={getCalendar} href="#">Download timetable as ICS</a><br />
            <a onClick={getPDF} href="#">Download PDF</a>
            <div>
            {props.data && <img id="post-image" src={props.data}/>}
            </div>
            <button type="button" className="btn btn-primary" id="switch-session-button"
                onClick={props.toggleSession}>Switch Sessions</button>
        </div>
        </div>
    );
}
