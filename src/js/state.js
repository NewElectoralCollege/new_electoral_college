function init() {
    let app = Elm.StateResults.init({
        node: $("#elm"),
        flags: new RegExp(
            "[?&]" + encodeURIComponent("state") + "=([^&]*)"
        ).exec(location.search)
            ? [getGet("state", "Georgia"), Number(getGet("year", 2020))]
            : [getCookie("state", "Georgia"), Number(getCookie("year", 2020))],
    });

    app.ports.initializePopovers.subscribe(function (parties) {
        MathJax.typeset();

        $('[data-toggle="popover"]').popover({
            content: $("#gallagher-desc"),
            html: true,
            trigger: "hover",
        });
    });
}
