window.onload = function () {
    $(".list-group-item").on("click", function (e) {
        e.preventDefault();
        $(this).tab("show");
    });
};
