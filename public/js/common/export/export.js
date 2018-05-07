import * as ReactModal from 'vendor/react-modal';

$(document).ready(function () {
  $('#nav-export').click(function () {
    openExportModal();
  });
});


/**
 * Creates and displays the Export modal content div.
 */
function openExportModal() {
    'use strict';
    var context = $('#courseography-header').attr('context');
    if (context !== 'graph') {
        ReactDOM.render(
            <ExportModal context='grid' session='fall' open={true}/>,
            document.getElementById('disclaimerDiv')).openModal();
    }
}

export var ExportModal = React.createClass({
    getInitialState: function () {
        return {
            data: '',
            otherSession: 'Spring'
        };
    },

    getImage: function() {
        if (this.props.context === 'graph') {
            this.getGraphImage();
        } else {
            this.getGridImage(this.props.session);
        }
    },

    getGraphImage: function() {
        var necessaryLS = new Object();
        for (var elem in localStorage) {
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
    },

    getGridImage: function (session) {
        var formattedSession = session.charAt(0).toUpperCase() + session.slice(1);
        $.ajax({
            url: 'timetable-image',
            data: {session: formattedSession, courses: localStorage.getItem("selected-lectures")},
            success: function (data) {
                this.setState({data: "data:image/png;base64," + data, otherSession: formattedSession === 'Fall' ? 'Spring' : 'Fall'});
            }.bind(this),
            error: function () {
                throw 'No image generated';
            }
        });
    },

    openModal: function() {
        this.setState({modalIsOpen: true}, this.getImage);
    },
    closeModal : function() {
        this.setState({modalIsOpen: false});
    },

    toggleSession: function () {
        this.getGridImage(this.state.otherSession);
    },

    render: function () {
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
});

var getCalendar = function() {
    $.ajax({
        type: "post",
        url: "calendar",
        data: {courses: localStorage.getItem("selected-lectures")},
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
};

var getPDF = function() {
    var necessaryLS = new Object();
    for (var elem in localStorage) {
        if (elem.substring(0,3).match(/^[a-zA-Z]+$/) && elem.substring(3,6).match(/^\d+$/)) {
            if (document.getElementById(elem)) {
                necessaryLS[elem] = localStorage.getItem(elem);
            }
        } else {
            necessaryLS[elem] = localStorage.getItem(elem);
        }
    }

    $.ajax({
        url: "timetable-pdf",
        data: {courses: localStorage.getItem("selected-lectures"), JsonLocalStorageObj: JSON.stringify(necessaryLS)},
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
};

var GraphImage = function (props) {
    return (
        <div>
        <div className='modal-header'>
            Export
        </div>
        <div className='modal-body'>
            <a onClick={getPDF}>Download PDF</a>
            <div>
            <img id="post-image" src={props.data}/>
            </div>
        </div>
        </div>
    );
};

var GridImage = function (props) {
    return (
        <div>
        <div className='modal-header'>
            Export
        </div>
        <div className='modal-body'>
            <a onClick={getCalendar}>Download timetable as ICS</a><br />
            <a onClick={getPDF}>Download PDF</a>
            <div>
            <img id="post-image" src={props.data}/>
            </div>
            <button type="button" className="btn btn-primary" id="switch-session-button"
                onClick={props.toggleSession}>Switch Sessions</button>
        </div>
        </div>
    );
};
