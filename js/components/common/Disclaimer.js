import React from "react";

// PRCOM: Change the design of the disclaimer and make it more obvious it is "on top"
/**
 * A React component representing the disclaimer popup
 */
export default class Disclaimer extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            hidden: localStorage.getItem('hide-disclaimer') === 'true'
        }
    }

    handleClose = () => {
        this.setState({hidden: true});
    }

    handleCheck = () => {
        // PRCOM: check whether the box is checked or not
        localStorage.setItem('hide-disclaimer', 'true');
    }

    render() {
        const timetable = <a href="https://timetable.iit.artsci.utoronto.ca/">Official Timetable</a>;
        const calendar = <a href="https://artsci.calendar.utoronto.ca/">Academic Calendar</a>;

        if (this.state.hidden) {
            return (null);
        } else {
            return (
                <div className="popup-banner">
                    <p><b>DISCLAIMER:</b> Both the {timetable} and {calendar} take precedence over the information
                    presented here. It's important that you double-check your course selection, prerequisites,
                    and your program plans. Some graph edges may represent a corequisite rather than a prerequisite.</p>
                    <label>
                        <input type="checkbox" class="dont-show-checkbox" onClick={this.handleCheck} />
                        Do not show this again
                    </label>
                    <button class="close-banner" onClick={this.handleClose}>&times;</button>
                </div>
            );
        }
    }
}
