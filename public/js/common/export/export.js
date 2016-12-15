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
    if (!(context === 'graph')) {
        ReactDOM.render(<ExportModal context = "grid" session = "fall"/>, document.getElementById('disclaimerDiv')).openModal();;
        
    }
} 

export var ExportModal = React.createClass({
    getInitialState: function () {
        return {
            data : "",
        };
    },

    componentDidMount: function() {
        if (this.props.context === "graph") {
            $.ajax({
            url: 'image',
            success: function (data) {
                this.setState({data: "data:image/png;base64," + data});
            }.bind(this),
            error: function () {
                throw 'No image generated';
            }
        });
    } else {
        var session = this.props.session.charAt(0).toUpperCase() + this.props.session.slice(1);
        $.ajax({
            url: 'timetable-image',
            data: {session: session},
            success: function (data) {
                this.setState({data: "data:image/png;base64," + data, session: session === 'Fall' ? 'Spring' : 'Fall'});
            }.bind(this),
            error: function () {
                throw 'No image generated';
            }
    });
    }
    },
    openModal: function() {
        this.setState({modalIsOpen: true});
    },
    closeModal : function() {
        this.setState({modalIsOpen: false});
    },
    render: function () {
        if (this.context === 'graph') {
            return (
                <div>
                <ReactModal
                    className='ModalClass'
                    overlayClassName='OverlayClass'
                    isOpen={this.state.modalIsOpen}
                    onRequestClose={this.closeModal}>
                    <GraphImage data = {this.state.data}/>   
                </ReactModal>
                </div> );
        } else {
            return (
                <div>
                <ReactModal
                    overlayClassName='OverlayClass'
                    isOpen={this.state.modalIsOpen}
                    onRequestClose={this.closeModal}>
                    <GridImage data = {this.state.data}/>   
                </ReactModal>
                </div> );

        }
       }
});
var GraphImage = React.createClass({
    render: function() {
        return(
            <div>
                <a href="calendar" target="_blank">Download ICS</a>
                <a href="timetable-pdf" target="_blank">Download PDF</a>
                <div>
                <img id="post-image" src={this.props.data}/>
                </div>
            </div>
        );
    }
})

var GridImage = React.createClass({
    render: function() {
        return(
            <div>
                <a href="calendar" target="_blank">Download ICS    </a>
                <a href="timetable-pdf" target="_blank">Download PDF</a>
                <div>
                <img id="post-image" src={this.props.data}/>
                </div>
            </div>
        );
    }
})