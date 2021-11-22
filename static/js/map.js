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
        $(".include#paths").html("");
    });

    app.ports.setCookieYear.subscribe(function (year) {
        document.cookie = "year=" + year;
    });
};
