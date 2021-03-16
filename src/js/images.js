window.onload = async function () {
    let app = Elm.Map.init({
        node: document.getElementById('main'),
        flags: Number(getCookie('year', 2020))
    });

    const hemicircle = await (await fetch('src/img/hemicircles/538_seats_electoral_college.svg')).text();

    app.ports.updateImages.subscribe(function (parties) {
        includeHTML();

        const hemicircle_div = document.querySelector('#hemicircle');
        hemicircle_div.innerHTML = hemicircle;
        
        let circles = Array.from(document.querySelectorAll('g#Results circle'));
        parties.forEach(function (party, n) {
            for (let i = 0; i < party.seats; i++) {
                circles[i].style.fill = party.color;
            }

            circles.splice(0, party.seats);
        })
    });

    app.ports.wipeContent.subscribe(function (data) {
        let p = document.querySelector('#paths');
        while (p.firstChild) {
            p.removeChild(p.firstChild);
        }
        app.ports.sendMsg.send(data);
    });
}