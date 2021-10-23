var init = function () {
    let app = Elm.StateResults.init({
        node: $("#elm"),
        flags: new RegExp(
            "[?&]" + encodeURIComponent("state") + "=([^&]*)"
        ).exec(location.search)
            ? [get_get("state", "Georgia"), Number(get_get("year", 2020))]
            : [get_cookie("state", "Georgia"), Number(get_cookie("year", 2020))],
    });
}

window.onload = function () {
    MathJax.typeset();

    $('[data-toggle="popover"]').popover({
        content: $("#gallagher-desc"),
        html: true,
        trigger: "hover",
    });
};
