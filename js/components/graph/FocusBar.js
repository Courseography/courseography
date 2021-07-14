import React from 'react';
import PropTypes from "prop-types";
import Focus from "./Focus";

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
            focusSelected: false,
            currFocus: ""
        };
    }

    handleClick = () => {
        if (this.state.open) {
            this.setState({open: false});
        } else {
            this.setState({open: true});
        }
    }

    generateFocusTabs = () => {
        return computerScienceFocusData.map((focus, i) => {
            const openDetails = this.state.currFocus == focus[0];
            return (
              <Focus
                key={i}
                pId={focus[0]}
                data-testid={"test-focus-" + i}
                focusName={focus[1]}
                order={i}
                openDetails={openDetails}
                highlightFocus={(id) => this.props.highlightFocus(id)}
              />
            )
          });
    }

    render() {
        let button, focuses;
        if (this.state.open) {
            button = <button className="focus-menu-toggle" onClick={this.handleClick}>⪡ CLOSE</button>;
            focuses = (
            <div className="focuses-list">
                {this.generateFocusTabs()}
            </div>
            );

        } else {
            button = <button className="focus-menu-toggle" onClick={this.handleClick}>FOCUSES ⪢</button>;
        }

        return (
            <div className="focus-menu-bar">
                {button}
                {focuses}
                <div style={{width: "150px", textAlign: "center"}}>
                    i
                </div>
            </div>
        );
    }
}

// TODO: check tabs vs spaces
FocusBar.propTypes = {
    focusBarEnabled: PropTypes.bool,
    highlightFocus: PropTypes.func,
};
