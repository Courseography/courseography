/* GLOBAL VARIABLES */
var nodeWidth = 40;
var nodeHeight = 32;
var xmlns = 'http://www.w3.org/2000/svg';
var svgDoc = null;
var nodeId = 0;
var mode = 'node-mode';
var nodeMoving = null;      // for movement and path creation
var prevX = -1;             // for movement
var prevY = -1;             // for movement
var nodeSelected = null;    // for adding text or changing colour
var startNode = null;       // for making paths
var curPath = null;         // the path currently being created
var elbowMoving = null;     // for movement of elbow joints
var regionId = 0;
var startPoint = null;
var regionMoving = null;
