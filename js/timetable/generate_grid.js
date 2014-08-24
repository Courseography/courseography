/*global $*/

/**
 * Generates the duplex timetable grid.
 */
function generateGrid() {
    var rowDiv = $("<div></div>").addClass("row");
    var timetableContainerDivFall = createTimeTableContainer();
    var timetableContainerDivSpring = createTimeTableContainer();
    var timetableTableFall = createTimeTable("F");
    var fallCaption = createCaption("Fall");
    var fallThead = createThead();
    var timetableTableSpring = createTimeTable("S");
    var springCaption = createCaption("Spring");
    var springThead = createThead();

    timetableTableFall.append(fallCaption)
        .append(fallThead);
    timetableTableSpring.append(springCaption)
        .append(springThead);

    appendHeaders(fallThead, springThead);
    appendTableRows(timetableTableFall, timetableTableSpring);

    timetableContainerDivFall.append(timetableTableFall);
    timetableContainerDivSpring.append(timetableTableSpring);
    rowDiv.append(timetableContainerDivFall)
        .append(timetableContainerDivSpring)
        .insertBefore($("#info"));
}


function createThead() {
    return $("<thead></thead>");
}


function createCaption(name) {
    return $("<caption></caption>").html(name);
}


function createTimeTable(suffix) {
    return $("<table></table>")
        .addClass("timetable table")
        .attr("id", "timetable-" + suffix);
}


function createTimeTableContainer() {
    return $("<div></div>")
        .addClass("col-md-6 col-xs-12 timetable-container");
}


function appendHeaders(fallThead, springThead) {
    var days = ["Mon", "Tue", "Wed", "Thu", "Fri"];
    fallThead.append($("<th></th>")
        .attr("id", "no-border")
        .addClass("timetable-time"));

    for (var j = 0; j < 5; j++) {
        fallThead.append($("<th></th>").html(days[j]));
        springThead.append($("<th></th>").html(days[j]));
    }

    springThead.append($("<th></th>")
        .attr("id", "no-border")
        .addClass("timetable-time"));
}


function appendTableRows(timetableTableFall, timetableTableSpring) {
    for (var i = 9; i < 22; i++) {
        var trFall = $("<tr></tr>");
        var trSpring = $("<tr></tr>");
        appendTableData(trFall, trSpring, i);
        timetableTableSpring.append(trSpring);
        timetableTableFall.append(trFall);
    }
}


function appendTableData(trFall, trSpring, time) {
    var weekPrefixArray = ["M", "T", "W", "R", "F"];

//    var adjustedTime = time === 12 ? 12 : time % 12;
    var adjustedTime = time + ':00';
    trFall.append($("<td></td>").addClass("timetable-time").html(adjustedTime));

    for (var k = 0; k < 5; k++) {
        trFall.append($("<td></td>")
            .attr("id", weekPrefixArray[k] + time + "F")
            .attr('in-conflict', 'false')
            .attr("satisfied", "true")
            .addClass("timetable-cell"));
        trSpring.append($("<td></td>")
            .attr("id", weekPrefixArray[k] + time + "S")
            .attr('in-conflict', 'false')
            .attr("satisfied", "true")
            .addClass("timetable-cell"));
    }

    trSpring.append($("<td></td>").addClass("timetable-time").html(adjustedTime));
}