// A form for feedback; currently disabled
function activateFeedbackForm() {
    $("#submit_btn").click(function () {
        var userName = $('input[name=name]').val();
        var userMessage = $('textarea[name=message]').val();
        var proceed = true;

        if (proceed) {
            var contactFormObject = $('#contact_form');
            var postData = {
                'userName': userName,
                'userMessage': userMessage
            };
            $.post('email.php', postData, function (data) {
                $("#result").hide()
                    .html('<div class="success">' + data + '</div>').slideDown();
                contactFormObject.find('input').val('');
                contactFormObject.find('textarea').val('');

            }).fail(function (err) {
                $("#result").hide()
                    .html('<div class="error">' + err.statusText + '</div>').slideDown();
            });
        }

    });

    $("#contact_form input, #contact_form textarea").keyup(function () {
        $("#contact_form input, #contact_form textarea").css('border-color', '');
        $("#result").slideUp();
    });
}