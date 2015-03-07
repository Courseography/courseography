function update300Categories() {
	'use strict';

	 if (specialist.filledTextboxes300 === specialist.textboxes300) {
	 	console.log('fulfilled!');
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        specialist.categoriesCompleted += 1;
    } else {
    	console.log('not fulfilled!');
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    if (major.filledTextboxes300 === major.textboxes300) {
    	console.log('fulfilled!');
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        major.categoriesCompleted += 1;
     } else {
     	console.log('not fulfilled!');
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
     }
}

function update400Categories() {
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
        categoriesCompleted.min += 1;
    } else {
		updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }
}