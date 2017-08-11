/* Draw page sidebar */

var Sidebar = React.createClass({
    //changeMode: function() {}

    render: function() {
        return (
        <div id="sidebar1" style={{width: 150, backgroundColor: "#007D7E"}}>
            <input id="course-code" className="course-code" name="course-code"
                placeholder="Course Code" autocomplete="off" type="text" size="10">
                </input>
            <div className="button">Add</div>
            <div className="mode clicked">NODE (n)</div>
            <div className="mode">PATH (p)</div>
            <div className="mode">REGION (r)</div>
            <div className="mode">MOVE (m)</div>
            <div className="mode">ERASE (e)</div>
            <div className="button">Finish (f)</div>
            <input className="jscolor" value="ff7878"
                size="15" autocomplete="off" style={{backgroundColor: "#FF7878",
                color: "#000000"}}></input>
            <table id="colour-table"><tbody>
                <tr><td></td><td></td><td></td><td></td><td></td></tr>
                <tr><td></td><td></td><td></td><td></td><td></td></tr>
                </tbody></table>
            <div className="button">SAVE</div>
            <input className="course-code" name="course-code"
                 placeholder="Enter area of study." autocomplete="off"
                 type="text" size="30">
                 </input>
            <div id="submit-graph-name" className="button">Search for department</div>
            <div id="json-data" className="json-data"></div>
        </div>
    )},

    statics: {
        selections: {
            NODE: 'n',
            PATH: 'p',
            REGION: 'r',
            MOVE: 'm',
            ERASE: 'e'
        }
    }
});

// <input className="course-code" type="text" value={this.state.value}/>
// <Button text="Finish (f)" \>
// <input id="select-colour" className="jscolor" value="ff7878"
//      size="15" autocomplete="off" style="background-image: none;\
//      background-color: rgb(255, 120, 120); color: rgb(0, 0, 0);"\>





// <div id="submit-graph-name" className="button">Search for department</div>
// <div id="json-data" className="json-data"></div>

// var Selector = React.createClass({
//     getInitialState: function() {
//         stored = getCookie('activeMode');
//         return {
//             mode: stored ? stored : this.selections.COURSE
//         };
//     },
//
//     getMode: function() {
//         return getCookie('activeMode');
//     },
//
//     getLinkClass: function(elementType) {
//         return (elementType === this.state.mode) ? 'side_bound' : 'side_free'
//     }
//
//     changeMode: function(e) {
//         var desired = Selector.selections[e.target.id.toUpperCase()]
//         this.setState(
//             {mode: desired},
//             function () {
//                 setCookie('activePost', desired);
//             }
//         )
//     },
//
//     render: function () {
//         return (
//             <nav id='side-panel-wrap'>
//                 <ul>
//                     <li id='course'
//                         className={this.getLinkClass(Selector.selections.COURSE)>
//                          <a onClick={this.changeMode}>Course (c)</a>
//                     </li>
//                     <li id='path'
//                         className={this.getLinkClass(Selector.selections.PATH)>
//                          <a onClick={this.changeMode}>Path (p)</a>
//                     </li>
//                     <li id='region'
//                         className={this.getLinkClass(Selector.selections.REGION)>
//                          <a onClick={this.changeMode}>Region (r)</a>
//                     </li>
//                     <li id='select'
//                         className={this.getLinkClass(Selector.selections.SELECT)>
//                          <a onClick={this.changeMode}>Select (s)</a>
//                     </li>
//                     <li id='delete'
//                         className={this.getLinkClass(Selector.selections.DELETE)>
//                          <a onClick={this.changeMode}>Delete (d)</a>
//                     </li>
//                 </ul>
//             </nav>
//         );
//     },
//
//     statics: {
//         selections: {
//             COURSE: 'c',
//             PATH: 'p',
//             REGION: 'r',
//             SELECT: 's',
//             DELETE: 'd'
//         }
//     }
// });

export default {Sidebar: Sidebar};
