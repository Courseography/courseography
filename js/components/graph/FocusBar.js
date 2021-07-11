import React from 'react';

export default class FocusBar extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            open: false,
            focusSelected: false
        }
    }

    handleClick = () => {
        if (this.state.open) {
            this.setState({open: false});
        } else {
            this.setState({open: true});
        }
    }

    render() {
        let button;
        if (this.state.open) {
            button = <button className="focus-menu-toggle" onClick={this.handleClick}>⪡ CLOSE</button>;
        } else {
            button = <button className="focus-menu-toggle" onClick={this.handleClick}>FOCUSES ⪢</button>
        }

        return (
            button
        );
    }
}