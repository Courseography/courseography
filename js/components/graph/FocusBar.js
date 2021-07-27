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

export default class FocusBar extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            open: false,
        };
    }

    toggleFocusBar = () => {
        if (this.state.open) {
            this.setState({open: false});
        } else {
            this.setState({open: true});
        }
    }

    generateFocusTabs = () => {
        return computerScienceFocusData.map((focus, i) => {
            const selected = this.props.currFocus == focus[0];

            return (
              <FocusTab
                key={focus[0]}
                pId={focus[0]}
                data-testid={"test-focus-" + i}
                focusName={focus[1]}
                selected={selected}
                highlightFocus={(id) => this.props.highlightFocus(id)}
              />
            )
          });
    }

    render() {
        let button, focuses;
        if (this.props.focusBarEnabled) {
            if(this.state.open) {
                button = <button className="focus-menu-toggle" onClick={this.toggleFocusBar}>⪡ CLOSE</button>;
                focuses = (
                    <div className="focuses-list">
                        {this.generateFocusTabs()}
                    </div>
                );
            } else {
                button = <button className="focus-menu-toggle" onClick={this.toggleFocusBar}>FOCUSES ⪢</button>;
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
