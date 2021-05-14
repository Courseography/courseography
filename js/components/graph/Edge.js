import PropTypes from "prop-types";
import React from "react";
import { refLookUp } from "../common/utils";

export default class Edge extends React.Component {
  constructor(props) {
    super(props);
    this.state = { status: "inactive" };
  }

  updateStatus = () =>{
    var source = refLookUp(this.props.source, this.props.svg);
    var target = refLookUp(this.props.target, this.props.svg);
    if (source === undefined || target === undefined) {
      return;
    }
    if (!source.isSelected() && target.state.status === "missing") {
      this.setState({ status: "missing" });
    } else if (!source.isSelected()) {
      this.setState({ status: "inactive" });
    } else if (!target.isSelected()) {
      this.setState({ status: "takeable" });
    } else {
      this.setState({ status: "active" });
    }
  }


  componentDidUpdate(prevProps, prevState) {
    // After each render, check if the edge's state has changed. If so,
    // notify the state of EdgeGroup with updateEdgeStatus.
    if (this.state.status !== prevState.status) {
      this.props.updateEdgeStatus(this.props.edgeID, this.state.status);
    }
  }

  render() {
    var pathAttrs = { d: "M" };
    this.props.points.forEach(p => {
      pathAttrs.d += p[0] + "," + p[1] + " ";
    });

    return (
      <path
        {...pathAttrs}
        className={this.props.className + " " + this.state.status}
        data-testid={`${this.props.source}->${this.props.target}`}
        markerEnd="url(#arrowHead)"
      />
    );
  }
}

Edge.propTypes = {
  className: PropTypes.string,
  edgeID: PropTypes.string,
  points: PropTypes.array,
  source: PropTypes.string,
  svg: PropTypes.object,
  target: PropTypes.string,
  updateEdgeStatus: PropTypes.func
};
