var row_div = React.createClass({
    render: function() {
      return(
        <div class="row">
          <timetablecontainer_div session={"F"} />
          <timetablecontainer_div session={"S"} />
        </div>
      );
    }
});

var timetablecontainer_div = React.createClass({
    render: function(){
      return(
      <div class="col-md-6 col-xs-12 timetable-container">
        <timetable_table session={this.props.session} />
      </div>
      );
    }
});

var timetable_table = React.createClass({
  render: function(){
    return(
      <table class={"timetable table"} id={"timetable-" + this.props.session}>
        <timetable_thead session = {this.props.session}/>
      </table>
    );
  }
});

var timetable_thead = React.createClass({
  render: function(){
    return(
      <thead>
        <timetable_th class_name = {"timetable-dummy-cell"} />
        <timetable_th class_name = {"term-name"} value = {this.props.session} />
        <timetable_th value = {"Mon"} />
        <timetable_th value = {"Tue"} />
        <timetable_th value = {"Wed"} />
        <timetable_th value = {"Thu"} />
        <timetable_th value = {"Fri"} />
      </thead>
    );
  }
});

var timetable_th = React.createClass({
  render: function(){
    return(
      <th class = {this.props.class_name}>
        {this.state.value};
      </th>
    );
  }
});

export function draw_grid() {
  ReactDOM.render(
      <row_div />,
      document.getElementsByClassName('row main'));
  }
