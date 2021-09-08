function init() {
    var app = Elm.Unsubscribe.init({
        node: document.getElementById("main"),
        flags: new RegExp(
            "[?&]" + encodeURIComponent("email") + "=([^&]*)"
        ).exec(location.search)
            ? getGet("email", "none")
            : getGet("email", "none"),
    });
}
