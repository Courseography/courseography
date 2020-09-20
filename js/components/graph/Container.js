import React from "react";
import Sidebar from "./Sidebar";

export default class Container extends React.Component {
  render() {
    return (
      <React.Fragment>
        <div id="react-graph" className="react-graph"></div>
        <Sidebar />
      </React.Fragment>
    )
  }
}
