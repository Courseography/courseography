import React from "react";
import Focus from "./Focus";

export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      toggled: false,
      hidden: true,
      focusHidden: true,
      focusActive: false,
      graphHidden: true,
      graphActive: false,
      sidebarFlipped: false,
    }
    this.toggleSidebar = this.toggleSidebar.bind(this);
    this.showFocuses = this.showFocuses.bind(this);
  }


  getFocusData() {
    const computerScienceFocusData = [
      ["sci", "Scientific Computing"],
      ["AI", "Artificial Intelligence"],
      ["NLP", "Natural Language Processing"],
      ["vision", "Computer Vision"],
      ["systems", "Computer Systems"],
      ["game", "Video Games"],
      ["HCI", "Human Computer Interaction"],
      ["theory", "Theory of Computation"],
      ["web", "Web Technologies"],
    ];
    const focusComponents = [];
    computerScienceFocusData.forEach((focus, i) => {
      focusComponents.push(
        <Focus key={i} pId={focus[0]} focusName={focus[1]} />
      )
    })
    return focusComponents;
  }

  toggleSidebar(location) {
    if (this.state.toggled) {
      this.setState({
        toggled: false,
        hidden: true,
        focusHidden: true,
        focusActive: false,
        graphHidden: true,
        graphActive: true,
        sidebarFlipped: false,
      })
      $("#sidebar").animate({ width: "40px" }, "fast", undefined, function() {
        $("#sidebar-icon").removeClass("flip");
      });
    } else if (!this.state.toggled && location === "button") {
      $("#sidebar").animate({ width: "400px" }, "fast", undefined, function() {
        $("#sidebar-icon").addClass("flip");
      });

      this.setState({
        toggled: true,
        hidden: false,
        graphActive: true,
        sidebarFlipped: true,
      })
      $("#graphs-nav").addClass("active");
    }
  }

  showFocuses() {
    this.setState({
      focusHidden: false,
      focusActive: true,
      graphHidden: true,
      graphActive: false,
    });
  }

  render() {
    const hiddenClass = this.state.hidden ? "hidden" : "";
    const focusActiveClass = this.state.focusActive ? "active" : "";
    const graphActiveClass = this.state.graphActive ? "active" : "";
    const focusClass = this.state.focusHidden ? "hidden" : "";
    const flippedClass = this.state.sidebarFlipped ? "flip" : "";
    return (
      <div>
        <div id="sidebar">
          <div id="fce" className={hiddenClass}>
            <div id="fcecount" className={hiddenClass}>FCE Count: 0.0</div>
            <button id="reset" className={hiddenClass}>Reset Graph</button>
          </div>
          <nav id="sidebar-nav">
            <ul>
              <li id="graphs-nav" className={graphActiveClass}>
                <a href="">Graphs</a>
              </li>
              <li id="focuses-nav" className={focusActiveClass} onClick={this.showFocuses}>
                <a href="">Focuses</a>
              </li>
            </ul>
          </nav>

          <div id="focuses" className={focusClass}>
            {this.getFocusData()}
          </div>
          <div id="graphs" className={hiddenClass}></div>
        </div>
        
        <div id="sidebar-button" onClick={() => this.toggleSidebar("button")}>
          <img id="sidebar-icon"
           className={flippedClass} 
           src="static/res/ico/sidebar.png"
          />
        </div>
      </div>
    )
  }
}
