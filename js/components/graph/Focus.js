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
    let focusClass = "focus";
    if (this.props.selected) {
      focusClass += " active-focus"
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
      <div className={focusClass} style={{'--background-color': this.props.color}}>
        <button id={this.props.pId} onClick={() => this.props.highlightFocus(this.props.pId)}>
          {this.props.focusName}
        </button>
        {detailsDiv}
      </div>
    )
  }
}

Focus.propTypes = {
  focusName: PropTypes.string,
  color: PropTypes.string,
  highlightFocus: PropTypes.func,
  selected: PropTypes.bool,
  pId: PropTypes.string
};
