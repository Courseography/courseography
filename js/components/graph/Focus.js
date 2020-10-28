import React from "react";
import PropTypes from "prop-types";
import * as focusInfo from "./sidebar/focus_descriptions.js";

export default class Focus extends React.Component {
  getDetailsInfo = () => {
    let detailsInfo;
    if (this.props.openDetails) {
      detailsInfo = {
        "detailsStyle": { height:"128px" },
        "detailsText": focusInfo[this.props.pId + "Description"]
      }
    } else {
      detailsInfo = {
        "detailsStyle": { height:"2px" },
        "detailsText": ""
      }
    }
    return detailsInfo;
  }

  render() {
    const detailsInfo = this.getDetailsInfo();
    const divId = this.props.pId + '-details';
    return (
      <div onClick={() => this.props.highlightFocus(this.props.pId)}>
        <p id={this.props.pId} className="focus">{this.props.focusName}</p>
        <div 
          id={divId}
          className="details"
          style={detailsInfo["detailsStyle"]}
          dangerouslySetInnerHTML={{__html: detailsInfo["detailsText"]}}
        />
      </div>
    )
  }
}

Focus.propTypes = {
  focusName: PropTypes.string,
  highlightFocus: PropTypes.func,
  openDetails: PropTypes.bool,
  pId: PropTypes.string
};
