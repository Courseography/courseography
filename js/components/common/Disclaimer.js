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
        let checkbox = document.getElementById("disclaimerCheck");
        if (checkbox.checked) {
            localStorage.setItem('hide-disclaimer', 'true');
        } else {
            localStorage.setItem('hide-disclaimer', 'false');
        }
    }

    render() {
        const timetable = <a href="https://timetable.iit.artsci.utoronto.ca/">Official Timetable</a>;
        const calendar = <a href="https://artsci.calendar.utoronto.ca/">Academic Calendar</a>;

        if (this.state.hidden) {
            return (null);
        } else {
            return (
                <div className="popup-card">
                    <button class="close-popup" onClick={this.handleClose}>&times;</button>
                    <div className="popup-content">
                        <h3>Disclaimer</h3>
                        <p>Please confirm your plans with official sources like the {timetable} and
                        {calendar} as they are more reliable. Also, some graph edges may represent corequisites.</p>
                        {/* <p>Both the {timetable} and {calendar} take precedence over the information presented here.
                        It's important that you double-check your course selection, prerequisites, and your
                        program plans.Some graph edges may represent a corequisite rather than a prerequisite.</p> */}
                        <button class="accept-popup" onClick={this.handleClose}>Understood</button>
                        <label>
                            <input type="checkbox" id="disclaimerCheck" class="dont-show-checkbox" onClick={this.handleCheck} />
                            Do not show this again
                        </label>
                    </div>
                </div>
            );
        }
    }
}
