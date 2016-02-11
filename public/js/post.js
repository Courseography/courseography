requirejs([
    'es6!post/post_page'],
    function (
        checkMyPost
    ) {

    $(document).ready(function () {
        ReactDOM.render(React.createElement(checkMyPost.CheckMyPost, null), document.getElementById('all_posts'));
        // openLastActiveTab();
    });
});