function init() {
    let download = function (link, name) {
        const a = $("a#downloader");
        a.attr("href", link);
        a.attr("download", name);
        a[0].click();
    };

    let name = function (link) {
        return link.replace("the_proposal/", "");
    };

    let app = Elm.ReadMore.init({ node: $("#elm") });

    app.ports.select.subscribe(function (module) {
        let elem = $("#block-" + module.id_);
        elem.attr("href", "#" + module.id_);

        $(".tab-content .tab-pane").removeClass("active");

        elem.tab("show");
    });

    app.ports.downloadLatex.subscribe(function (module) {
        download(module.latex, name(module.latex));
    });

    app.ports.downloadPdf.subscribe(function (module) {
        download(module.pdf, name(module.pdf));
    });
}
