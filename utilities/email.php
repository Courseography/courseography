<?php
if($_POST)
{
    if(!isset($_SERVER['HTTP_X_REQUESTED_WITH']) AND strtolower($_SERVER['HTTP_X_REQUESTED_WITH']) != 'xmlhttprequest') {
        die();
    } 
    
    $toEmail       = "cs.toronto.courseplanner@gmail.com";
    $subject        = 'Feedback from course planner!!!';
    
    if(/*!isset($_POST["userName"])*/ !isset($_POST["userMessage"])) {
        die();
    }
    
    $userName        = filter_var($_POST["userName"], FILTER_SANITIZE_STRING);
    $userMessage     = filter_var($_POST["userMessage"], FILTER_SANITIZE_STRING);

    $headers = 'New Message!' . " From: " . $userName .
    '  X-Mailer: PHP/' . phpversion();
    
    @$sentMail = mail($toEmail, $subject, $userMessage .'  -'.$userName, $headers);
    
    if(!$sentMail) {
        header('HTTP/1.1 500 Could not send mail! Try clicking the button as many times as you can. Maybe it will work!');
        exit();
    } else{
        // This prints the output to the results div.
        echo 'Hi '. $userName .', Thank you for your feedback! ';
    }
}
?>