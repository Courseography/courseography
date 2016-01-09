requirejs([
    'es6!post/post_components'],
    function (
        postComponents
    ) {

    $(document).ready(function () {
        ReactDOM.render(React.createElement(postComponents.SpecialistPost, null), document.getElementById('div_specialist'));
    });
});