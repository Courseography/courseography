requirejs([
    'es6!post/post_components'],
    function (
        postComponents
    ) {

    $(document).ready(function () {
        ReactDOM.render(React.createElement(postComponents.CheckMyPost, null), document.getElementById('all_posts'));
        openLastActiveTab();
    });
});