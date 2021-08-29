// Used instead of str.replace(). Replaces all instances of the needle, not just one.
function replace(needle, replace, haystack) {
    while (haystack.includes(needle)) {
        haystack.replace(needle, replace);
    }
    return haystack;
}

// Handle Cookies
function getCookie(name, fallback) {
    try {
        return document.cookie.match(
            new RegExp("(^| )" + name + "=([^;]+)")
        )[2];
    } catch {
        document.cookie = name + "=" + fallback;
        return fallback;
    }
}

// Get GET
function getGet(key, fallback) {
    let get = new RegExp("[?&]" + encodeURIComponent(key) + "=([^&]*)").exec(
        location.search
    );
    return get ? decodeURIComponent(get[1]) : fallback;
}

function includeHTML() {
    let z, i, elmnt, file, xhttp;
    z = $(".include");
    for (i = 0; i < z.length; i++) {
        elmnt = z[i];
        file = elmnt.getAttribute("w3-include-html");
        if (file) {
            xhttp = new XMLHttpRequest();
            xhttp.onreadystatechange = function () {
                if (this.readyState == 4) {
                    if (this.status == 200) {
                        elmnt.insertAdjacentHTML(
                            "afterbegin",
                            this.responseText
                        );
                    }
                    if (this.status == 404) {
                        console.log("Hello");
                    }
                    elmnt.removeAttribute("w3-include-html");
                    includeHTML();
                }
            };
            xhttp.open("GET", file, true);
            xhttp.send();
            return;
        }
    }
}
