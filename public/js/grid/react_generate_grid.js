
/** Holds the containers of the Fall and Spring timetables */
var Row = React.createClass({
  render: function(){
    return(
      <div className="row">
        <TimetableContainer session={"F"} />
        <TimetableContainer session={"S"} />
      </div>
    );
  }
});

/** The container specifies formatting for all of the elements wrapped inside,
    (for example, every element inside a container will follow the same margin rules) */
var TimetableContainer = React.createClass({
    render: function(){
      return(
      <div className="col-md-6 col-xs-12 timetable-container">
        <Timetable session={this.props.session} />
      </div>
      );
    }
});

/** A <table> element for the specified session*/
var Timetable = React.createClass({
  render: function(){
    return(
      <table className={"timetable table"} id={"timetable-" + this.props.session}>
        <TimetableHeader session = {this.props.session}/>
        <TimetableBody session = {this.props.session}/>
      </table>
    );
  }
});

/** Describes what the header of a table should look like, based off the session. */
var TimetableHeader = React.createClass({
  render: function(){
      let tableHeader = null;
      let dayCells = [<th>Mon</th>, <th>Tue</th>, <th>Wed</th>, <th>Thu</th>, <th>Fri</th>];
      if(this.props.session == "F"){
        tableHeader = <thead>
                        <th className="timetable-dummy-cell"></th>
                        <th className="term-name">Fall</th>
                        {dayCells}
                      </thead>;
      }else if(this.props.session == "S"){
        tableHeader = <thead>
                        {dayCells}
                        <th className="term-name">Spring</th>
                        <th className="timetable-dummy-cell"></th>
                      </thead>;
      }
      return(
        tableHeader
      );
  }
});

/** Fills the body of the table. For each hour from 8 o'clock to 22 o'clock,
    there will be two rows displaying information for the half hours */
var TimetableBody = React.createClass({
  render: function(){
    let TableRows = [];
    for (var i = 8; i < 22; i++) {
      let TableRowHour = <TimetableRow session={this.props.session} time={i}/>
      let TableRowHalfHour = <TimetableRow session={this.props.session} time={i+0.5}/>
      TableRows.push(TableRowHour);
      TableRows.push(TableRowHalfHour);
    }
    return(
      <tbody>
        {TableRows}
      </tbody>
    );
}});

/** Describes what a row in the Timetable should look like,
    based off of the session and time.
    Each row has seven cells -- <dayCell> (used for weekday information),
    <timeCell> (displays time of the row),
    and <dummyCell> (used for formatting).
    Notice that the ordering in which the 'cell' elements are rendered
    is important, as it allows the generated HTML to comply with the CSS.*/
var TimetableRow = React.createClass({
  render: function(){

    let TableData = [];
    let weekPrefixArray = ['M', 'T', 'W', 'R', 'F'];
    let dummyCell = <td className="timetable-dummy-cell"></td>;

    // If the time describes a row that starts at the hour (for example, at 8:00)
    if (this.props.time % 1 === 0) {
      var adjustedTime = (this.props.time === 12 ? 12 : this.props.time % 12) + ':00';
      let timeCell = <td className="timetable-time" rowSpan="2">{adjustedTime}</td>
      for (var i = 0; i < 5; i++) {
        let dayCell = <td id={'' + weekPrefixArray[i] + this.props.time + '-0' + this.props.session}
                          data-in-conflict="false"
                          data-satisfied={"true"}
                          rowSpan="2"
                          className="timetable-cell">
                      </td>;
        TableData.push(dayCell);
      }
      if(this.props.session == 'F'){
        TableData.unshift(timeCell);
        TableData.unshift(dummyCell);
      }else if(this.props.session == 'S'){
        TableData.push(timeCell);
        TableData.push(dummyCell);
      }

    // Otherwise, the will time describe a row that starts at the half hour (for example, at 8:30)
    }else{
      for (var i = 0; i < 5; i++) {
        let dayCell = <td id={'' + weekPrefixArray[i] + this.props.time.toString().replace('.', '-') + this.props.session + 'H'}
                          data-in-conflict="false"
                          data-satisfied={"true"}
                          rowSpan="1"
                          className="timetable-cell timetable-half-cell">
                      </td>;
        TableData.push(dayCell);
      }
      if(this.props.session == 'F'){
        TableData.unshift(dummyCell);
      }else if(this.props.session == 'S'){
        TableData.push(dummyCell);
      }
    }
    return(
      <tr>
        {TableData}
      </tr>
    );
  }
});

/** Renders everything wrapped inside the "Row" element */
export function drawTimetable() {
  ReactDOM.render(
    <Row />,
    document.getElementById('grid-body').getElementsByClassName('col-md-8 col-xs-12 col-md-pull-2')[0]
  );
}
