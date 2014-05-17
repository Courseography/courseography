function set_cookie (cookie_name, cookie_value, lifespan_in_days, valid_domain) {
    console.log("setting cookie...");
    var domain_string = valid_domain ?
                       ("; domain=" + valid_domain) : '' ;
    document.cookie = cookie_name +
                       "=" + encodeURIComponent( cookie_value ) +
                       "; max-age=" + 60 * 60 *
                       24 * lifespan_in_days +
                       "; path=/" + domain_string ;
}

function getCookie(cname)
{
var name = cname + "=";
var ca = document.cookie.split(';');
for(var i=0; i<ca.length; i++) 
  {
  var c = ca[i].trim();
  if (c.indexOf(name)==0) {
  return c.substring(name.length,c.length);
    console.log("cookie" + c.substring(name.length,c.length));
  }
return "inactive";
}