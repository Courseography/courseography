var trapScroll;

/*
 * Adapted from http://codepen.io/LelandKwong/pen/edAmn. Will look into http://jscrollpane.kelvinluck.com/.
 */
 (function($){  

    trapScroll = function(){

        var trapElement;
        var scrollableDist;
        var trapClassName = "trapScroll-enabled";
        var trapSelector = "#course-select";
        
        var trapWheel = function(e){
            if (!$("body").hasClass(trapClassName)) {
                return;
            } else {        
                var curScrollPos = trapElement.scrollTop();
                var wheelEvent = e.originalEvent;
                var dY = wheelEvent.deltaY;

                // only trap events once we've scrolled to the end
                // or beginning
                if ((dY>0 && curScrollPos >= scrollableDist) ||
                    (dY<0 && curScrollPos <= 0)) {

                    return false;
                }
            }
        };

        $(document)
        .on("wheel", trapWheel)
        .on("mouseleave", trapSelector, function(){

            $("body").removeClass(trapClassName);
        })
        .on("mouseenter", trapSelector, function(){        

            trapElement = $(this);
            var containerHeight = trapElement.outerHeight();
                var contentHeight = trapElement[0].scrollHeight; // height of scrollable content
                scrollableDist = contentHeight - containerHeight;
                
                if (contentHeight>containerHeight) {
                    $("body").addClass(trapClassName); 
                }       
        });       
    };

})($);

$.event.special.hoverintent = {
    setup: function() {
        $( this ).bind( "mouseover", jQuery.event.special.hoverintent.handler );
    },
    teardown: function() {
        $( this ).unbind( "mouseover", jQuery.event.special.hoverintent.handler );
    },
    handler: function( event ) {
        var currentX, currentY, timeout,
        args = arguments,
        target = $( event.target ),
        previousX = event.pageX,
        previousY = event.pageY;

        function track( event ) {
            currentX = event.pageX;
            currentY = event.pageY;
        }

        function clear() {
            target
            .unbind( "mousemove", track )
            .unbind( "mouseout", clear );
            clearTimeout( timeout );
        }

        function handler() {
            var prop,
            orig = event;

            if ( ( Math.abs( previousX - currentX ) +
                Math.abs( previousY - currentY ) ) < 7 ) {
                clear();

            event = $.Event( "hoverintent" );
            for ( prop in orig ) {
                if ( !( prop in event ) ) {
                  event[ prop ] = orig[ prop ];
                }
            }
                // Prevent accessing the original event since the new event
                // is fired asynchronously and the old event is no longer
                // usable (#6028)
                delete event.originalEvent;

                target.trigger( event );
            } else {
                previousX = currentX;
                previousY = currentY;
                timeout = setTimeout( handler, 100 );
            }
        }
     
        timeout = setTimeout( handler, 100 );
        target.bind({
            mousemove: track,
            mouseout: clear
        });
    }
};

function setAccordion() {
    $("#course-select").accordion({heightStyle: "content", collapsible: true, active: false/*, event: "click hoverintent"*/});
}

/*
 * Previously used to eliminate duplicate times, such as R10 for 263.
 */
// function returnUniqueElements(array) {
//     var sortedArray = array.sort();
//     var result = []
//     for (var i = 0; i < array.length - 1; i++) {
//         if (sortedArray[i + 1] !== sortedArray[i]) {
//             result.push(sortedArray[i]);
//         }
//     }
//     result.push(sortedArray[array.length-1]);
//     return result;
// }