var init = function () {
    var app = Elm.Unsubscribe.init({
        node: $("#elm"),
        flags: new RegExp(
            "[?&]" + encodeURIComponent("email") + "=([^&]*)"
        ).exec(location.search)
            ? get_get("email", "none")
            : get_get("email", "none"),
    });
}
