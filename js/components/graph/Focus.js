import React from "react";
import PropTypes from "prop-types";
import * as focusInfo from "./sidebar/focus_descriptions.js";

const COLORS = ["#d472c7", "#ce73d3", "#c974d2", "#c672d2", "#bd71d3", "#b575d5", "#b072d3", "#ab72d3", "#a172d2"];

export default class Focus extends React.Component {
  getDetailsInfo = () => {
    let detailsStyle = { height:"128px" };
    let detailsText = focusInfo[this.props.pId + "Description"];

    return { "detailsStyle": detailsStyle, "detailsText": detailsText };
  }

  render() {
    const divId = this.props.pId + '-details';
    const backgroundColor = COLORS[this.props.order];
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
      <div className="focus" style={{background: backgroundColor, borderColor: backgroundColor}}>
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
  order: PropTypes.number,
  highlightFocus: PropTypes.func,
  selected: PropTypes.bool,
  pId: PropTypes.string
};
