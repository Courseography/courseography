<?php
if($_POST)
{
    if(!isset($_SERVER['HTTP_X_REQUESTED_WITH']) AND strtolower($_SERVER['HTTP_X_REQUESTED_WITH']) != 'xmlhttprequest') {
        die();
    } 
    
    $to_Email       = "istewartbinks@gmail.com";
    $subject        = 'Ah!! My email from Somebody out there...';

    // Name is... a placebo... for now...
    if(/*!isset($_POST["userName"])*/ !isset($_POST["userMessage"]))
    {
        die();
    }

   $user_Message     = filter_var($_POST["userMessage"], FILTER_SANITIZE_STRING);
    
    // Need to.. insert.. name.
    $headers = 'From: ' . "rn" .
    'New Message!' . "rn" .
    'X-Mailer: PHP/' . phpversion();
    
    @$sentMail = mail($to_Email, $subject, $user_Message .'  -', $headers);
    
    if(!$sentMail)
    {
        header('HTTP/1.1 500 Could not send mail! Try clicking the button as many times as you can. Maybe it will work!');
        exit();
    }else{
        // For when I... add.. username capability...
        //echo 'Hi '.$user_Name .', Thank you for your email! ';
        echo 'Thanks for your feedback!';
    }
}
?>