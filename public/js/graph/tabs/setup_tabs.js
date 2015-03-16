/**
 * Sets up the tabs.
 */
function createTabs() {
    'use strict';

    $('.infoTabs').tabs({
        activate: function (e, ui) {
            e.currentTarget.blur();
        }
    });
    $('.focusTabs').tabs({
        active: false,
        collapsible: true,
        activate: function (e, ui) {
            var name = ui.newTab.attr('aria-controls');
            if (name) {
                name = name.substr(0, name.length - 7);
                updateActiveFocus(name);
            } else {
                updateActiveFocus('');
            }
        }
    });
    $('.postTypeTabs, .postTabs').tabs({
        active: 0,
        activate: function (e, ui) {
            e.currentTarget.blur();
        }
    });

    createTimetable();
    createTimetableSearch();
}
