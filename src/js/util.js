const fix_change = function (text) {
    return text.replace(/(\+0(?!.)|\+0.00)/g, "<i class='steady'>&#9644;</i> 0").replace("+-", "<i class='decrease'>&#9660;</i> ").replace("+", "<i class='increase'>&#9650;</i> ")
}

const party_colors = {
    "Democratic": "#3333ff",
    "Republican": "#ff3333",
    "Libertarian": "#FED105",
    "Green": "#17aa5c",
    "Peace and Freedom Party": "#00FF00",
    "Reform": "#6A287E",
    "Ross Perot": "#6A287E",
    "Independent": "#969696"
}

function includeHTML() {
    let z, i, elmnt, file, xhttp;
    z = document.getElementsByTagName("*");
    for (i = 0; i < z.length; i++) {
        elmnt = z[i];
        file = elmnt.getAttribute("w3-include-html");
        if (file) {
            xhttp = new XMLHttpRequest();
            xhttp.onreadystatechange = function() {
                if (this.readyState == 4) {
                    if (this.status == 200) {elmnt.innerHTML = this.responseText;}
                    if (this.status == 404) {console.log("Hello");}
                    elmnt.removeAttribute("w3-include-html");
                    includeHTML();
                }
            }
            xhttp.open("GET", file, true);
            xhttp.send();
            return;
        }
    }
}

function getPathList() {
    let array = Array.from(document.querySelectorAll("#map path"));
    let output = [];
    array.forEach((item, n) => {
        if (!item.id.startsWith('path') && !item.id.startsWith("Puerto-Rico"))
            output.push(item);
    })
    output.sort((a, b) => (a.id.localeCompare(b.id) != -1) ? 1 : -1);
    return output;
}

// The reason why some of the charts on the main results page are shifted is because there are multiple of those parties in the JSONs