/**
 * The click function when a focus is clicked.
**/
$('.focus').click(function(e){
    'use strict';

    var id = $(this).attr('id');

    if ($('#' + id + '-details').css('height') === '180px') {
        clearFocus();
        $('ellipse.spotlight').remove();
        setMouseCallbacks();
        $('#' + id + '-details').animate({height: '2px'}, 'fast');
        $('#' + id + '-details').html("");
    } else {
        $('.details').css('height', '2px');
        updateActiveFocus(id);
        $('#' + id + '-details').animate({height: '180px'}, 'fast');
        $('#' + id + '-details').html(window[id + 'Description']);
    } 
});

/**
 * Click function to load CSC graph
**/
$('#graph-csc').click(function(e) {
    'use-strict';

    getRemote();

    buildGraph();

    FCEPrerequisiteCourses = [CSC318, CSC454];

    // Set width of FCE count
    var w = $('.infoTabs').width() - $('.tabList').outerWidth() - 1;
    $('#FCECountDiv').width(w + 'px');

    // Create tabs
    createTabs();

    // Set mouse callbacks
    setMouseCallbacks();

    // Initialize interface
    initializeGraphSettings();
    
    // Update credit count in nav bar
    updateNavGraph();

    fillFCECount();

    // Uncomment to enable the feedback form (must also be displayed in html)
    // activateFeedbackForm();
    // Uncomment to enable graph dragging
    // enableGraphDragging();
});