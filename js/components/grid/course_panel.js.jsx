export class CoursePanel extends React.Component {
  render() {
    return (
      <div id="course-select-wrapper" className="col-md-2 col-xs-6">
        <ul className="trapScroll-enabled" id="course-select"
            onClick={() => alert("test")}>
          <li id="clear-all">
            <h3>Clear All</h3>
          </li>
        </ul>
      </div>
    );
  }
}
