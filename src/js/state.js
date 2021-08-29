function init() {
    var app = Elm.StateResults.init({
        node: document.getElementById("main"),
        flags: new RegExp(
            "[?&]" + encodeURIComponent("state") + "=([^&]*)"
        ).exec(location.search)
            ? [getGet("state", "Georgia"), Number(getGet("year", 2020))]
            : [getCookie("state", "Georgia"), Number(getCookie("year", 2020))],
    });
}
