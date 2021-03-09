const roundTen = function (n) { 
    return Math.floor(n / 10) * 10;
};

const real_results = {
    "Democratic": [297, 49, 13, 112, 370, 379, 266, 251, 365, 332, 232, 306],
    "Republican": [240, 489, 525, 426, 168, 159, 271, 286, 173, 206, 304, 232],
    "Libertarian": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "Green": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "Peace and Freedom Party": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "Reform": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "Ross Perot": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "Independent": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "Free Libertarian": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
}

const new_results = {
    "Democratic": [297, 223, 218, 247, 233, 269, 263, 257, 288, 275, 261, 276],
    "Republican": [240, 277, 320, 291, 194, 225, 264, 279, 247, 260, 260, 261],
    "Libertarian": [0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 13, 1],
    "Green": [0, 0, 0, 0, 0, 1, 11, 0, 0, 0, 1, 0],
    "Peace and Freedom Party": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "Reform": [0, 0, 0, 0, 103, 43, 0, 0, 0, 0, 0, 0],
    "Ross Perot": [0, 0, 0, 0, 103, 43, 0, 0, 0, 0, 0, 0],
    "Independent": [1, 37, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
    "Free Libertarian": [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
}

const detailed_results_starting_point = `
<td class="detailed-results-cell">
    <table class="detailed-results">
        <tr>
            <th rowspan="2">State</th>
            <th colspan="3"></th>
            <th colspan="2"></th>
        </tr>
        <tr>
            <th>Votes</th>
            <th>%</th>
            <th>+/-</th>
            <th>Electors</th>
            <th>+/-</th>
        </tr>
    </table>
</td>
`

function sendMeTo(state) {
    document.querySelector('#state-name').innerHTML = state;
    return;
    document.cookie = "state=" + state.id + "; expires=; path=/;";
    window.location = "state.html";
}

let year;
let total_votes = 0;

let map;
let hemicircle;

async function ld() {
    const census = roundTen(year - 1);
    map = await (await fetch('src/img/maps/us_electoral_college_' + census.toString() + '.svg')).text();
    hemicircle = await (await fetch('src/img/hemicircles/538_seats_electoral_college.svg')).text();

    document.querySelectorAll("#election").forEach(function (span, n) { span.innerHTML = year; });
    
    document.querySelector("#map").innerHTML = map;
    document.querySelector("#hemicircle").innerHTML = hemicircle;

    let totals = {};
    let party_list = Array();

    const paths = getPathList();

    document.querySelector('#row-for-detailed-results').innerHTML = detailed_results_starting_point + detailed_results_starting_point;
    const detailed_results_tables = document.querySelectorAll(".detailed-results");

    const svg = document.querySelector("svg");
    
    const recurseFunction = async function (json, old_json, n) {
        path = paths[n];
        path.setAttribute("mouseover", "sendMeTo(" + path.id + ")");
        path.addEventListener("mouseover", function () { sendMeTo(path.id); } );
        //document.querySelector(`rect.bb.${path.id}`).setAttribute("onclick", "sendMeTo(" + path.id + ")");
    
        const state_json = json[path.id.replace("-", " ")];
        const parties = state_json["parties"];

        //makeCircles(path.id, state_json.stats.total_seats, svg);
        let state_circles = Array.from(document.querySelectorAll('#map circle.' + path.id));

        state_circles.sort((a, b) => (
            (parseFloat(a.getAttribute("cy")) > parseFloat(b.getAttribute("cy"))) ||
            (parseFloat(a.getAttribute("cy")) == parseFloat(b.getAttribute("cy")) && parseFloat(a.getAttribute("cx")) > parseFloat(b.getAttribute("cx")))
        ) ? 1 : -1);

        const previous_year = year - ((year == 1976) ? 0 : 4);
        const state_json_previous = old_json[path.id.replace("-", " ")];

        parties.sort((a, b) => (a.votes < b.votes) ? 1 : -1);
        parties.forEach(async (party, i) => {
            for (let c = 0; c < party.seats; c++) {
                try {
                    state_circles[0].style.fill = party_colors[party.name];
                } catch {
                    console.log(path.id + " has too many circles");
                }
                state_circles.shift();
            }
            try {
                total_votes += party.votes;
                totals[party.name]['seats'] += party.seats;
                totals[party.name]['votes'] += party.votes;
            } catch {
                totals[party.name] = {
                    'name': party.name,
                    'seats': 0,
                    'votes': 0
                };
                party_list[party_list.length] = party.name;
                totals[party.name]['seats'] = party.seats;
                totals[party.name]['votes'] = party.votes;
            }
            if (party.name == "Democratic" || party.name == "Republican" || (["Reform", "Ross Perot"].includes(party.name) && [1992, 1996].includes(year))) {
                let i = (party.name == "Republican") ? 1 : ((["Reform", "Ross Perot"].includes(party.name)) ? 2 : 0);
                let previous_party_results;
                for (let x = 0; x < state_json_previous.parties.length; x++)
                    if (state_json_previous.parties[x].name == party.name || (party.name == "Reform" && state_json_previous.parties[x].name == "Ross Perot"))
                        previous_party_results = state_json_previous.parties[x];
                detailed_results_tables[i].innerHTML += `
                    <tr>
                        <td>${path.id.replace("-", " ").replace("-", " ")}</td>
                        <td>${party.votes.toString().replace(/(.)(?=(\d{3})+$)/g,'$1,')}</td>
                        <td>${(party.votes / state_json.stats.total_votes * 100).toFixed(2)}%</td>
                        <td>${fix_change("+" + (((party.votes / state_json.stats.total_votes) - (previous_party_results.votes / state_json_previous.stats.total_votes)) * 100).toFixed(2))}%</td>
                        <td>
                            ${party.seats} / ${state_json.stats.total_seats}
                            <div style="border:1px solid black; width: 100px; height: 1.15em; padding: 0">
                                <div style="background-color:${party_colors[party.name]}; width:${party.seats / state_json.stats.total_seats * 100}%; height: 101%; display: inline-block;"></div>
                            </div>
                        </td>
                        <td>${fix_change("+" + (party.seats - previous_party_results.seats).toString())}</td>
                    </tr>
                `;
            }
        });

        if (paths.length == n + 1) {
            const national_circles = Array.from(document.querySelectorAll('g#Results circle'));

            const table = document.querySelector("#single-results");
            while (table.childNodes.length > 2) table.removeChild(table.lastChild);
        
            party_list.sort((a, b) => (totals[a].votes < totals[b].votes) ? 1 : -1);
            party_list.forEach(function (party, n) {
                for (let na = 0; na < totals[party].seats; na++) {
                    national_circles[0].setAttribute("fill", party_colors[totals[party].name]);
                    national_circles.shift();
                }
                if (
                    totals[party].seats > 0 ||
                    totals[party].votes / total_votes >= 0.1
                ) table.innerHTML += `
                    <tr>
                        <td class="color" style="background-color: ${party_colors[party]}"></td>
                        <td>${party}</td>
                        <td>n/a</td>
                        <td>${totals[party].votes.toString().replace(/(.)(?=(\d{3})+$)/g,'$1,')}</td>
                        <td>${(totals[party].votes / total_votes * 100).toFixed(2)}%</td>
                        <td>${totals[party].seats}</td>
                        <td>${real_results[party][year / 4 - 494]}</td>
                        <td>${fix_change("+" + (totals[party].seats - new_results[party][year / 4 - 495]).toString())}</td>
                    </tr>
                `;
            })
        }

        if (paths.length != n + 1) recurseFunction(json, old_json, n+1);
    };

    $.ajax({
        type: 'Get',
        url: '/new_electoral_college/src/js/getJson.php?year=' + (year - 4),
    }).done(function (old_response) {
        $.ajax({
            type: 'Get',
            url: '/new_electoral_college/src/js/getJson.php?year=' + year,
        }).done(function (response) {
            recurseFunction(JSON.parse(response), JSON.parse(old_response), 0);
        }).fail( function (error) {
            console.log(error);
        })
    }).fail( function (error) {
        console.log(error);
    })
}

function changeYear(up) {
    const inc = (up) ? 4 : -4;
    document.cookie = "year=" + (year + inc).toString() + "; expires=; path=/;";
    year += inc;
    if (year == 2020) {
        document.querySelector("#rightArrow").style.display = "none";
    } else if (year == 1976) {
        document.querySelector("#leftArrow").style.display = "none";
    }
    ld();
}

function makeCircles(state, number, svg) {
    const r = 5.5;
    const state_path = document.querySelector(`rect.bb.${state}`);
    state_path.style.strokeOpacity = 0;
    const bb = {
        "x": Number(state_path.getAttribute("x")),
        "y": Number(state_path.getAttribute("y")),
        "width": Number(state_path.getAttribute("width")),
        "height": Number(state_path.getAttribute("height"))
    }
    const center = [bb["x"] + (bb["width"] / 2), bb["y"] + (bb["height"] / 2)];

    let coords = [];
    let rows = 0;
    let columns = 0;
    let increment = false;
    let add_html = '';

    if (Number.isInteger(Math.sqrt(number)) && bb["width"] < (r * 2 * Math.sqrt(number))) {
        rows = Math.sqrt(number);
        columns = Math.sqrt(number);
    } else if (number < 4) {
        if (bb["width"] > bb["height"]) {
            rows = 1;
            columns = number;
        } else {
            rows = number;
            columns = 1;
        }
    } else if (number % 2 == 0 && number < 9) {
        if (bb["width"] > bb["height"]) {
            rows = 2;
            columns = number / 2;
        } else {
            rows = number / 2;
            columns = 2;
        }
    } else if (number % 2 != 0 && number < 9) {
        rows = 2;
        columns = Math.round(number / 2);
        increment = true;
    } else if (true) {
        if (bb["width"] > bb["height"]) {
            columns = Math.round(Math.min(bb["width"] / (r * 2), ((bb["width"] > bb["height"] * 2) ? 100 : Math.sqrt(number))));
            rows = Math.floor(number / columns) + ((Number.isInteger(number / columns) ? 0 : 1));
        } else {
            rows = Math.round(Math.min(bb["height"] / (r * 2), (bb["height"] > bb["width"] * 2) ? 100 : Math.sqrt(number)));
            columns = Math.floor(number / rows) + ((Number.isInteger(number / columns) ? 0 : 1));
        }
        increment = true;
    } else {
        return "Hello";
    }

    const offsetx = r * 2 * (columns / 2 - 0.5);
    const offsety = r * 2 * (rows / 2 - 0.5);
    let x = center[0] - offsetx;
    let y = center[1] - offsety;
    let in_this_row = 0;

    while (coords.length != number) {
        coords.push([x, y]);
        in_this_row += 1;

        if (columns <= in_this_row) {
            x = center[0] - offsetx;
            y += r * 2;
            if (number - coords.length < in_this_row && increment) {
                x += r * (in_this_row - (number - coords.length));
            }
            in_this_row = 0;
        } else {
            x += r * 2;
        }
    }

    coords.forEach(function (coord_set, n) {
        add_html += `
            <circle
                r="${r}"
                class="${state}"
                cx="${coord_set[0]}"
                cy="${coord_set[1]}"
                id="ellipse2400"
                style="opacity:1;fill:#dddddd;fill-opacity:1;stroke:#000000;stroke-width:${(1.1 / 7.089) * r};stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" />
        `;
    })

    /*svg.innerHTML += `
        <circle
            r="${r}"
            cx="${center[0]}"
            cy="${center[1]}"
            id="ellipse2400"
            style="opacity:1;fill:#ff0000;fill-opacity:0.5;stroke:#000000;stroke-width:0;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" />
        `;*/

    svg.innerHTML += add_html;
}

window.onload = async function () {
    
    try {
        year = Number(document.cookie.match(new RegExp('(^| )' + "year" + '=([^;]+)'))[2]);
    } catch {
        year = 2020;
        document.cookie = "year=2020; expires=; path=/;";
    }

    ld();
}