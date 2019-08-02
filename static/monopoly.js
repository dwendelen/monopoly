$(document).ready(function() {
    var loc = window.location;
    var websocket = new WebSocket('ws://'+ loc.host + '/socket');
    var outer = $("#outer");
});