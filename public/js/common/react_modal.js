function openReactModal(courseCode) {
    
    var courseName = getCourseTitle(courseCode);
    var formattedName = formatCourseName(courseCode);
    var courseVideoUrls = getCourseVideoUrls(formattedName);
    
    React.render(
        <ModalContent course={formattedName[0]}/>,
        document.getElementById('modal-content-container')
    );
}

var ModalContent = React.createClass({
    render: function() {
        return (
            <div>
                <Description course={this.props.course} />
            </div>
        );
    }
});

var Description = React.createClass({
  getInitialState: function() {
    return {
        course: [],
        sessions: []
    };
  },

  componentDidMount: function(){
      //This loads the course json
      $.ajax({
        url: 'course',
        data: {name: this.props.course},
        dataType: 'json',
        success: function(data) {
            if (this.isMounted()) {
                this.setState({course: data});
                
                console.log(data);
                
                //Not sure if there is a cleaner way to do this
                //This is getting the session times
                var lectures = data.fallSession.lectures
                                         .concat(data.springSession.lectures)
                                         .concat(data.yearSession.lectures);
                var sessions = [];
                $.each(lectures, function( index, value ) {
                    sessions.push(value.code+value.session + "-" + value.section +": " + value.timeStr);
                    console.log( value.code+value.session + "-" + value.section +": " + value.timeStr );
                });
                this.setState({sessions: sessions});
                console.log("this.state.course.videoUrls", this.state.course.videoUrls);
            }
        }.bind(this),
        error: function(xhr, status, err) {
            console.error('course-info', status, err.toString());
        }.bind(this)
    });
  },
    
  render: function() {
    //TODO: add video URLs but need to ask David how it works
    return (
      <div>
        <p>{this.state.course.description}</p>
        <p><strong>Prerequisite: </strong>{this.state.course.prereqString}</p>
        <p><strong>Distribution Requirement Status: </strong>{this.state.course.distribution}</p>
        <p><strong>Breadth Requirement: </strong>{this.state.course.breadth}</p>
        <p><strong>Timetable: </strong></p>
        {this.state.sessions.map(function(session) {
            return <p>{session}</p>;
        })}
                                   
        <div id="course-video-div">
            <video id="course-video" className="video-js vjs-default-skin" controls="" preload="auto">
                <source src={this.state.course.videoUrls} type="video/mp4"/>
            </video>
        </div>
      </div>
    );
  }
});