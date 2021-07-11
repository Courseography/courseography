import React from "react";
import PropTypes from "prop-types";
import * as focusInfo from "./sidebar/focus_descriptions.js";

export default class Focus extends React.Component {
  getDetailsInfo = () => {
    let detailsStyle = { height:"128px" };
    let detailsText = focusInfo[this.props.pId + "Description"];

    return { "detailsStyle": detailsStyle, "detailsText": detailsText };
  }

  render() {
    const divId = this.props.pId + '-details';
    let detailsInfo, detailsDiv;
    if (this.props.selected) {
      detailsInfo = this.getDetailsInfo();
      detailsDiv = <div
                    id={divId}
                    className="details"
                    style={detailsInfo["detailsStyle"]}
                    dangerouslySetInnerHTML={{__html: detailsInfo["detailsText"]}}
                  />;
    } else {
      detailsDiv = null;
    }

    return (
      <div onClick={() => this.props.highlightFocus(this.props.pId)}>
        <button id={this.props.pId} className="focus">{this.props.focusName}</button>
        {detailsDiv}
      </div>
    )
  }
}

Focus.propTypes = {
  focusName: PropTypes.string,
  highlightFocus: PropTypes.func,
  selected: PropTypes.bool,
  pId: PropTypes.string
};
