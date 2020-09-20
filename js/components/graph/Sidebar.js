import React from "react";
// import * as sidebarDivs from "./sidebar/sidebar_divs.js";
// import * as focusInfo from "./sidebar/focus_descriptions";


export default class Sidebar extends React.Component {
  render() {
    return (
      <div>
        <div id="sidebar">
        </div>
        <div id="sidebar-button">
          <img id="sidebar-icon" src="static/res/ico/sidebar.png"></img>
        </div>
      </div>
    )
  }
}
