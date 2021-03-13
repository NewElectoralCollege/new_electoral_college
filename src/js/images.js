window.onload = async function () {
    let app = Elm.Map.init({
        node: document.getElementById('main'),
        flags: 2020
    });

    const hemicircle = await (await fetch('src/img/hemicircles/538_seats_electoral_college.svg')).text();

    const hemicircle_div = document.querySelector('#hemicircle');

    app.ports.updateImages.subscribe(function (parties) {
        //const map = await (await fetch('src/img/maps/us_electoral_college_' + census.toString() + '.svg')).text();
        
        //document.querySelector('#map').innerHTML = map;
        hemicircle_div.innerHTML = hemicircle;
        
        let circles = Array.from(document.querySelectorAll('g#Results circle'));

        parties.forEach(function (party, n) {
            for (let i = 0; i < party.seats; i++) {
                circles[i].style.fill = party.color;
            }

            circles.splice(0, party.seats);
        })
    });
}