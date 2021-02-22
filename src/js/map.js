const roundTen = function (n) { 
    return Math.floor(n / 10) * 10;
};

const party_colors = {
    "Democratic": "#3333ff",
    "Republican": "#ff3333",
    "Libertarian": "#000000"
}

let totals = {};
let party_list = Array();

function finalize() {
    const national_circles = Array.from(document.querySelectorAll('g#Results circle'));
    console.log(party_list);
    party_list.forEach(async function (party, n) {
        for (let na = 0; na < totals[party].seats; na++) {
            national_circles[na].setAttribute("fill", party_colors[totals[party].name]);
        }
    })
}

async function ld() {
    let year;
    try {
        year = Number(document.cookie.match(new RegExp('(^| )' + "year" + '=([^;]+)'))[2]);
    } catch {
        year = 2020;
        document.cookie = "year=2020; expires=; path=/;";
    }

    const census = roundTen(year - 1);
    const map = await (await fetch('src/img/maps/us_electoral_college_' + census + '.svg')).text();
    const hemicircle = await (await fetch('src/img/hemicircles/538_seats_electoral_college.svg')).text();
    document.querySelector("#map").insertAdjacentHTML('afterbegin', map);
    document.querySelector("#hemicircle").insertAdjacentHTML('afterbegin', hemicircle);   

    document.querySelectorAll("#map path").forEach(async function (path, n) {
        let call = await fetch('data/' + year + '/' + path.id.replace("-", " ").replace("-", " ") + '.json');
        if (call.status == 200) {
            const parties = (await (call).json())["parties"];

            let state_circles = Array.from(document.querySelectorAll('#map ellipse.' + path.id));

            state_circles.sort((a, b) => (
                (parseFloat(a.getAttribute("cy")) > parseFloat(b.getAttribute("cy"))) ||
                (parseFloat(a.getAttribute("cy")) == parseFloat(b.getAttribute("cy")) && parseFloat(a.getAttribute("cx")) > parseFloat(b.getAttribute("cx")))
            ) ? 1 : -1);

            parties.forEach(function (party, i) {
                for (let c = 0; c < party.seats; c++) {
                    state_circles[0].style.fill = party_colors[party.name];
                    state_circles.shift();
                }
                try {
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
            });
        }
        
    });
    return finalize;

}

window.onload = async function () {
    let promise = new Promise(function(resolve, reject) { 
        resolve(ld());
    });
    (await promise)();
}

