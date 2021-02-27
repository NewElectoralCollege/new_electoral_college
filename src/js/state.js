async function makeStateResults(state_name, party, table) {
    let last_seats = 0;
    let last_percentage = 0;
    for (let i = 1976; i < 2021; i += 4) {
        const state = await (await fetch('data/' + i.toString() + '/' + state_name.replace("-", " ").replace("-", " ") + '.json')).json();
        state.parties.sort((a, b) => (b.name == party) ? 1 : -1);
        table.innerHTML += `
            <tr>
                <td>${i}</td>
                <td>${state.parties[0].votes.toString().replace(/(.)(?=(\d{3})+$)/g,'$1,')}</td>
                <td>${(state.parties[0].votes / state.stats.total_votes * 100).toFixed(2)}%</td>
                <td>${fix_change("+" + (((state.parties[0].votes / state.stats.total_votes) - last_percentage) * 100).toFixed(2))}%</td>
                    <td>
                        ${state.parties[0].seats} / ${state.stats.total_seats}
                        <div style="border:1px solid black; width: 100px; height: 1.15em; padding: 0">
                            <div style="background-color:${party_colors[party]}; width:${state.parties[0].seats / state.stats.total_seats * 100}%; height: 101%; display: inline-block;"></div>
                        </div>
                    </td>
                <td>${fix_change("+" + (state.parties[0].seats - last_seats).toString())}</td>
            </tr>
        `;
        last_seats = state.parties[0].seats;
        last_percentage = state.parties[0].votes / state.stats.total_votes;
    }
}

if (window.location.href.split("/").pop() == "state.html") {
    let year = 2020;
    let state_name = "Georgia";
    try {
        year = Number(document.cookie.match(new RegExp('(^| )' + "year" + '=([^;]+)'))[2]);
    } catch {
        document.cookie = "year=2020; expires=; path=/;";
    }
    try {
        state_name = document.cookie.match(new RegExp('(^| )' + "state" + '=([^;]+)'))[2];
    } catch {
        document.cookie = "state=Georgia; expires=; path=/;";
    }

    window.onload = async function () {
        const state_data = await (await fetch('data/' + year.toString() + '/' + state_name.replace("-", " ").replace("-", " ") + '.json')).json();
        const circles_g = document.querySelector("svg g#circles");
        const bar_g = document.querySelector("svg g#bar");
        const results_table = document.querySelector("table#single-results");

        state_data.parties.sort((a, b) => (a.votes < b.votes) ? 1 : -1);

        let coords = [];
        let angle = 0;
        let assigned = 0;
        let bar_width = 0;
        let bar_coords = [100, 370];

        for (let n in state_data.parties) {
            let party = state_data.parties[n];

            for (let i = 0; i < party.seats; i++) {
                angle = (Math.PI / (state_data.stats.total_seats) * (assigned + state_data.stats.total_seats + 0.5));
                coords = [
                    (10 * 35) * Math.cos(angle) + 450,
                    (10 * 35) * Math.sin(angle) + 375
                ];
    
                circles_g.innerHTML += `
                    <circle
                        cx="${coords[0]}"
                        cy="${coords[1]}"
                        r="10"
                        id="${assigned}"
                        style="fill:${party_colors[party.name]}"
                    />
                `;
                assigned++;
            }

            bar_width = party.votes / state_data.stats.total_votes * 700;
            bar_g.innerHTML += `
                <rect
                    x="${bar_coords[0]}"
                    y="${bar_coords[1]}"
                    width="${bar_width}"
                    height="50"
                    style="fill:${party_colors[party.name]};"
                />
            `;
            bar_coords[0] += bar_width;

            if ((party.votes / state_data.stats.total_votes >= 0.01 || party.seats > 0) && party.name != "Other") {
                results_table.innerHTML += `
                    <tr>
                        <td class="color" style="background-color: ${party_colors[party.name]}"></td>
                        <td>${party.name}</td>
                        <td>n/a</td>
                        <td>${party.votes.toString().replace(/(.)(?=(\d{3})+$)/g,'$1,')}</td>
                        <td>${(party.votes / state_data.stats.total_votes * 100).toFixed(2)}%</td>
                        <td>${party.seats - ((party.extra_seat) ? 1 : 0)}</td>
                        <td>${party.extra_votes.toString().replace(/(.)(?=(\d{3})+$)/g,'$1,') + ((party.extra_seat) ? " <i class='fa' style='color:green'>&#xf058;</i>" : "")}</td>
                        <td>${party.seats}</td>
                    </tr>
                `;
            }
        }

        const tables = document.querySelectorAll("#state-results");
        makeStateResults(state_name, "Democratic", tables[0]);
        makeStateResults(state_name, "Republican", tables[1]);
    }
}