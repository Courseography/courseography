import React from "react"
import PropTypes from "prop-types"
import Disclaimer from "../common/Disclaimer"
import { Graph } from "./Graph"
import FocusBar from "./FocusBar"

export default class Container extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      currFocus: null,
      fceCount: 0,
      graphName: "",
      graphs: [],
    }
    this.graph = React.createRef()
  }

  UNSAFE_componentWillMount() {
    this.getLocalGraph()
  }

  componentDidMount() {
    fetch("/graphs")
      .then(res => res.json())
      .then(
        graphsData => {
          this.setState({
            graphs: graphsData.sort((a, b) => (a.id > b.id ? 1 : -1)),
          })
        },
        () => {
          throw "No graphs in database"
        }
      )

    // Enable "Export" link
    document.getElementById("nav-export")?.addEventListener("click", () => {
      this.graph.current.openExportModal()
    })
  }

  updateGraph = graphName => {
    this.setState({ graphName: graphName.replace("-", " ") })
  }

  getLocalGraph = () => {
    // Gets graph from local storage, if it exists
    let graphName
    const params = new URL(document.location).searchParams
    const urlSpecifiedGraph = params.get("dept")

    // HACK: Temporary workaround for giving the statistics department a
    // link to our graph.
    // Should be replaced with a more general solution.
    if (urlSpecifiedGraph === "sta") {
      graphName = "Statistics"
    } else if (urlSpecifiedGraph !== null) {
      graphName = "Computer Science"
    } else {
      graphName = localStorage.getItem("active-graph") || "Computer Science"
    }
    this.setState({ graphName: graphName })
    return graphName
  }

  setFCECount = credits => {
    this.setState({ fceCount: credits })
  }

  incrementFCECount = credits => {
    this.setState({ fceCount: this.state.fceCount + credits })
  }

  highlightFocus = id => {
    if (this.state.currFocus === id) {
      this.setState({
        currFocus: null,
      })
    } else {
      this.setState({
        currFocus: id,
      })
    }
  }

  render() {
    return (
      <div>
        <Disclaimer />
        <Graph
          ref={this.graph}
          currFocus={this.state.currFocus}
          edit={this.props.edit}
          fceCount={this.state.fceCount}
          getLocalGraph={this.getLocalGraph}
          graphName={this.state.graphName}
          incrementFCECount={this.incrementFCECount}
          initialDrawMode="draw-node"
          setFCECount={this.setFCECount}
          start_blank={this.props.start_blank}
          graphs={this.state.graphs}
          updateGraph={this.updateGraph}
        />
        <FocusBar
          focusBarEnabled={this.state.graphName === "Computer Science"}
          highlightFocus={this.highlightFocus}
          currFocus={this.state.currFocus}
        />
      </div>
    )
  }
}

Container.propTypes = {
  start_blank: PropTypes.bool,
  edit: PropTypes.bool,
}
