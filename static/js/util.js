// Used instead of str.replace(). Replaces all instances of the needle, not just one.
var replace = function (needle, replace, haystack) {
    while (haystack.includes(needle)) {
        haystack.replace(needle, replace);
    }

    return haystack;
};

// Handle Cookies
var get_cookie = function (name, fallback) {
    try {
        return document.cookie.match(
            new RegExp("(^| )" + name + "=([^;]+)")
        )[2];
    } catch {
        document.cookie = name + "=" + fallback;
        return fallback;
    }
};

// Get GET
var get_get = function (key, fallback) {
    const get = new RegExp("[?&]" + encodeURIComponent(key) + "=([^&]*)").exec(
        location.search
    );

    return get ? decodeURIComponent(get[1]) : fallback;
};

// Validate forms

var validate_form = function (form) {
    return function (event) {
        if (form.checkValidity() === false) {
            event.preventDefault();
            event.stopImmediatePropagation();
        }
        form.classList.add("was-validated");
    };
};

var validate_forms = function () {
    const forms = $(".needs-validation");

    var validation = Array.prototype.filter.call(forms, function (form) {
        form.addEventListener("submit", validate_form(form), false);
    });
};

var identity = function (a) {
    return a;
};

// Include HTML

var includeHTML = function (alter = identity) {
    let z, i, elmnt, file, xhttp, act_text;
    z = $(".include");
    for (i = 0; i < z.length; i++) {
        elmnt = z[i];
        file = elmnt.getAttribute("nec-include-html");
        if (file) {
            xhttp = new XMLHttpRequest();
            xhttp.onreadystatechange = function () {
                if (this.readyState == 4) {
                    if (this.status == 200) {
                        act_text = alter(this.responseText);

                        elmnt.insertAdjacentHTML("afterbegin", act_text);
                    }
                    if (this.status == 404) {
                        console.log("Hello");
                    }
                    elmnt.removeAttribute("nec-include-html");
                    includeHTML();
                }
            };
            xhttp.open("GET", file, true);
            xhttp.send();
            return;
        }
    }
};
