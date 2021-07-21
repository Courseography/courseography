import React from 'react';
import ReactModal from 'react-modal';
import PropTypes from "prop-types";
import Focus from "./Focus";
import * as focusInfo from "./sidebar/focus_descriptions.js";

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

const COLORS = ["#d472c7", "#ce73d3", "#c974d2", "#c672d2", "#bd71d3", "#b575d5", "#b072d3", "#ab72d3", "#a172d2"];
const SelectedColors = ["#e67ad7", "#e380e8", "#de82e8", "#d67be3", "#d07de8", "#c781eb", "#bf7ce6", "#b87be3", "#b07ce6"];

export default class FocusBar extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            open: false,
            selectedFocus: [],
            showFocusModal: false
        };
    }

    handleClick = () => {
        if (this.state.open) {
            this.setState({open: false});
        } else {
            this.setState({open: true});
        }
    }

    selectFocus = id => {
        if (this.state.selectedFocus[0] === id) {
            this.setState({
              selectedFocus: []
            });
          } else {
            this.setState({
              selectedFocus: computerScienceFocusData.find(focus => focus[0] == id)
            });
          }
        this.props.highlightFocus(id);
    }

    generateFocusTabs = () => {
        return computerScienceFocusData.map((focus, i) => {
            const selected = this.props.currFocus == focus[0];

            return (
              <Focus
                key={i}
                pId={focus[0]}
                data-testid={"test-focus-" + i}
                focusName={focus[1]}
                color={selected ? SelectedColors[i] : COLORS[i]}
                selected={selected}
                selectFocus={(id) => this.selectFocus(id)}
              />
            )
          });
    }

    getDetailsInfo = () => {
        let detailsText = focusInfo[this.state.selectedFocus[0] + "Description"];
        return {__html: detailsText};
    }

    changeShowFocusModal = value => {
        this.setState({
            showFocusModal: value
        });
    }

    render() {
        let button, focuses, focusModal, focusInfoButton;
        let detailsInfo;
        if (this.state.selectedFocus.length > 0) {
            detailsInfo = this.getDetailsInfo();
        }

        if (this.props.focusBarEnabled) {
            if(this.state.open) {
                button = <button className="focus-menu-toggle" onClick={this.handleClick}>⪡ CLOSE</button>;
                focuses = (
                    <div className="focuses-list">
                        {this.generateFocusTabs()}
                    </div>
                );
                if(this.state.selectedFocus.length > 0) {
                    focusModal = (
                        <ReactModal className='modal-class'
                            overlayClassName='overlay'
                            isOpen={this.state.showFocusModal}
                            onRequestClose={() => this.changeShowFocusModal(false)}
                            ariaHideApp={false}
                        >
                            <div className='modal-header'>
                                {this.state.selectedFocus[1]}
                            </div>
                            <div className='modal-body' dangerouslySetInnerHTML={detailsInfo}>
                            </div>
                        </ReactModal>
                    );
                    focusInfoButton = (
                        <button onClick={() => this.changeShowFocusModal(true)}>
                            i
                        </button>
                    );
                }
            } else {
                button = <button className="focus-menu-toggle" onClick={this.handleClick}>FOCUSES ⪢</button>;
            }
        }

        return (
            <div className="focus-menu-bar">
                {button}
                {focuses}
                <div className="focus-info">
                    {focusModal}
                    {focusInfoButton}
                </div>
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
