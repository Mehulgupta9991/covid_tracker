/*
* - Define variables
* - Calculate the document height and set the offset to a quarter of that value
* - Add the event listeners for scroll and click
*/
var btt = document.getElementById("back-to-top"),
    body = document.body,
    docElem = document.documentElement,
    offset = 100,
    scrollPos, docHeight,
    isFireFox = (navigator.userAgent.toLowerCase() == "firefox");

docHeight = Math.max(body.scrollHeight, body.offsetHeight, docElem.clientHeight, docElem.scrollHeight, docElem.offsetHeight);
if (docHeight != "undefined") {
    offset = docHeight / 4;
}

// add scroll event listener
window.addEventListener("scroll", function(event) {
    scrollPos = body.scrollTop || docElem.scrollTop;
    btt.className = (scrollPos > offset) ? "visible" : "";
});

// add click event listener
btt.addEventListener("click", function(event) {
    if (isFirefox) {
        docElem.scrollTop = 0;
    } else {
        body.scrollTop = 0;
    }
});
