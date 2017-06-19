/* Draw page sidebar */

var Sidebar = React.createClass({
    getInitialState: function() {
        return {
            mode: Sidebar.selection.NONE
        }
    }

    changeSelection: function() {
        //TODO
    }

    changeColor: function() {

    }

    onKeyDown: function (event) {
        
    }

    render: function() {

        return (
            <div>
                <button onClick={changeSelection}>Course (c)</button>
                <button onClick={changeSelection}>Path (p)</button>
                <button onClick={changeSelection}>Region (r)</button>
                <button onClick={changeSelection}>Select (s)</button>
                <button onClick={changeSelection}>Delete (d)</button>
                <button onClick={changeColor}>Colour (l)</button>
                <Search></Search>
            </div>
        )
    }

    statics: {
        selection: {
            NONE: ' '
            COURSE: 'c',
            PATH: 'p',
            REGION: 'r',
            SELECT: 's',
            DELETE: 'd',
            COLOR: 'l'
        }
    }
});

export default {Sidebar: Sidebar};
