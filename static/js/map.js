var port = function () {
    let app = Elm.Map.init({
        node: $("#elm"),
        flags: Number(get_cookie("year", 2020)),
    });

    app.ports.updateImages.subscribe(function () {
        $(".include#paths").attr(
            "nec-include-html",
            "static/img/us_electoral_college_2010.svg"
        );

        includeHTML(function (string) {
            return string.replace(new RegExp("<\\?xml(.|\\s)*?<\\/defs>"), "");
        });
    });

    app.ports.wipeContent.subscribe(function () {
        let p = $("#paths");

        while (p.firstChild) {
            p.removeChild(p.firstChild);
        }
    });

    app.ports.setCookieYear.subscribe(function (year) {
        document.cookie = "year=" + year;
    });
};
