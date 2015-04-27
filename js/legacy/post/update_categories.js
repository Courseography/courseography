'use strict'

/**
 * Updates categories for required courses in each POSt
 * @param {object} post The POSt that you are updating categories for.
 * @param {string} name Name of course
 */
export function updateReqsCategory(post, name) {
    'use strict';

    for (var i = 0; i < post.reqs.length; i++) {
        var category = $('#' + name + '_' + post.reqs[i].toLowerCase())[0].getElementsByClassName('code')[0];
        if (activeCourses.indexOf(post.reqs[i]) !== -1) {
            activateCourse(post.reqs[i]);
            updateCategory(category, 'fulfilled');
            post.categoriesCompleted += 1;
            if (post.reqs[i] === 'Calc1') {
            	post.creditCount += 1;
            } else {
            	post.creditCount += 0.5;
            }
        } else { // if the category is not completed
            deactivateCourse(post.reqs[i]);
            updateCategory(category, 'not fulfilled');
        }
    }
}


/**
 * Updates 300 level category
 */
export function update300Categories() {
    'use strict';

    if (specialist.filledTextboxes300 === specialist.textboxes300) {
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        specialist.categoriesCompleted += 1;
    } else {
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    if (major.filledTextboxes300 === major.textboxes300) {
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        major.categoriesCompleted += 1;
    } else {
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }
}


/**
 * Updates 400 level category
 */
export function update400Categories() {
    'use strict';

    if (specialist.filledTextboxes400 === specialist.textboxes400) {
        updateCategory($('#spec_400')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        specialist.categoriesCompleted += 1;
    } else {
        updateCategory($('#spec_400')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    if (major.filledTextboxes400 === major.textboxes400) {
        updateCategory($('#maj_400')[0].getElementsByClassName('code')[0], 'fulfilled');
        major.categoriesCompleted += 1;
    } else {
        updateCategory($('#maj_400')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }
}


/**
 * Updates Extra level category
 */
function updateExtraCategories() {
    'use strict';

    if (specialist.filledTextboxesExtra === specialist.textboxesExtra) {
        updateCategory($('#spec_extra')[0].getElementsByClassName('code')[0], 'fulfilled');
        specialist.categoriesCompleted += 1;
    } else {
    	updateCategory($('#spec_extra')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    if (major.filledTextboxesExtra === major.textboxesExtra) {
        updateCategory($('#maj_extra')[0].getElementsByClassName('code')[0], 'fulfilled');
        major.categoriesCompleted += 1;
    } else {
    	updateCategory($('#maj_extra')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    if (minor.filledTextboxesExtra === minor.textboxesExtra) {
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        minor.categoriesCompleted += 1;
    } else {
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }
}


/**
 * Updates Inquiry category
 */
function updateInqCategory () {
    'use strict';

    if (specialist.activeInq === 1) {
        updateCategory($('#spec_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        specialist.categoriesCompleted += 1;
    } else {
        updateCategory($('#spec_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    if (major.activeInq === 1) {
        updateCategory($('#maj_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        major.categoriesCompleted += 1;
    } else {
        updateCategory($('#maj_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }
}
