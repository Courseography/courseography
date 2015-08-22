/**
 * Adapted from http://codepen.io/LelandKwong/pen/edAmn.
 */
define(function() {
    return { trap_scroll: 
        function () {
            var trapElement;
            var scrollableDist;
            var trapClassName = 'trapScroll-enabled';
            var trapSelector = '#course-select';

            var trapWheel = function(e){
                if (!$('body').hasClass(trapClassName)) {
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
                .on('wheel', trapWheel)
                .on('mouseleave', trapSelector, function() {
                    $('body').removeClass(trapClassName);
                })
                .on('mouseenter', trapSelector, function() {
                    trapElement = $(this);
                    var containerHeight = trapElement.outerHeight();
                    var contentHeight = trapElement[0].scrollHeight; // height of scrollable content
                    scrollableDist = contentHeight - containerHeight;

                    if (contentHeight > containerHeight) {
                        $('body').addClass(trapClassName);
                    }
                });
        }
    }
});