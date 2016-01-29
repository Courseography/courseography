requirejs([
    'es6!post/post_components'],
    function (
        postComponents
    ) {

    $(document).ready(function () {
        ReactDOM.render(React.createElement(postComponents.SpecialistPost, null), document.getElementById('div_specialist'));
        ReactDOM.render(React.createElement(postComponents.MajorPost, null), document.getElementById('div_major'));
        ReactDOM.render(React.createElement(postComponents.MinorPost, null), document.getElementById('div_minor'));
    });
});