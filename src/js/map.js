const roundTen = function (n) { 
    return Math.floor(n / 10) * 10;
};

window.onload = async function () {
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
}