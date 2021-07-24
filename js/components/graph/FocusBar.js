import React from 'react';
import PropTypes from "prop-types";
import FocusTab from "./FocusTab.js";

// These lists are in reverse order to what ends up appearing on the screen
const computerScienceFocusData = [
    ["web", "Web Technologies"],
    ["theory", "Theory of Computation"],
    ["HCI", "HumanComp Interaction"],
    ["game", "Video Games"],
    ["systems", "Computer Systems"],
    ["vision", "Computer Vision"],
    ["NLP", "Computational Linguistics"],
    ["AI", "Artificial Intelligence"],
    ["sci", "Scientific Computing"],
  ];

// PRCOM: try a different method
const COLORS = ["#d472c7", "#ce73d3", "#c974d2", "#c672d2", "#bd71d3", "#b575d5", "#b072d3", "#ab72d3", "#a172d2"];
const selectedColors = ["#e67ad7", "#e380e8", "#de82e8", "#d67be3", "#d07de8", "#c781eb", "#bf7ce6", "#b87be3", "#b07ce6"];
// PRCOM: make the tab bigger instead

export default class FocusBar extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            open: false,
        };
    }

    //PRCOM: pick a better function name
    handleClick = () => {
        if (this.state.open) {
            this.setState({open: false});
        } else {
            this.setState({open: true});
        }
    }

    selectFocus = id => {
        this.props.highlightFocus(id);
    }

    generateFocusTabs = () => {
        return computerScienceFocusData.map((focus, i) => {
            const selected = this.props.currFocus == focus[0];

            return (
              <FocusTab
                key={i}
                pId={focus[0]}
                data-testid={"test-focus-" + i}
                focusName={focus[1]}
                color={selected ? selectedColors[i] : COLORS[i]}
                selected={selected}
                selectFocus={(id) => this.selectFocus(id)}
              />
              // PRCOM: you could use onClick here instead of inside
            )
          });
    }

    render() {
        let button, focuses;
        if (this.props.focusBarEnabled) {
            if(this.state.open) {
                button = <button className="focus-menu-toggle" onClick={this.handleClick}>⪡ CLOSE</button>;
                focuses = (
                    <div className="focuses-list">
                        {this.generateFocusTabs()}
                    </div>
                );
            } else {
                button = <button className="focus-menu-toggle" onClick={this.handleClick}>FOCUSES ⪢</button>;
            }
        }

        // PRCOM: remove variables
        return (
            <div className="focus-menu-bar">
                {button}
                {focuses}
            </div>
        );
    }
}

// TODO: check tabs vs spaces
FocusBar.propTypes = {
    focusBarEnabled: PropTypes.bool,
    highlightFocus: PropTypes.func,
    currFocus: PropTypes.string,
};
