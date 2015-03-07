function updateActiveCourses() {
	'use strict';

	for (areaName in areas) {
		if (areas.hasOwnProperty(areaName)) {
			for (i = 0; i = areas.areaName.length; i ++) {
				if (getCookie(areas.areaName[i]) === 'active') {
					activeCourses.push(areas.areaName[i]);
				}
			}
		}
	}
}