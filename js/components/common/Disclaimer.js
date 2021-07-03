import React from "react";

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
                        <p>Please make sure to confirm your course selections and prerequisites with
                         official sources like the {timetable} and {calendar} as they are more reliable and up-to-date. </p>
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
