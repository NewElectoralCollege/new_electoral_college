function port() {
    let app = Elm.Map.init({
        node: $("#elm"),
        flags: Number(getCookie("year", 2020)),
    });

    app.ports.updateImages.subscribe(function () {
        $(".include#paths").attr(
            "w3-include-html",
            "src/img/us_electoral_college_2010.svg"
        );

        includeHTML();
    });

    app.ports.wipeContent.subscribe(function (data) {
        let p = $("#paths");

        while (p.firstChild) {
            p.removeChild(p.firstChild);
        }

        app.ports.sendMsg.send(data);
    });
}
