var c = document.getElementById("theCanvas");
var ctx = c.getContext("2d");
ctx.fillStyle = "#FF00FF";
ctx.fillRect(0, 0, 80, 100);

$( "#target" ).click(function() {
  alert( "Handler for .click() called." );
});

//$( "#myCanvas" ).click(function() {
 // drawRect();
//});
