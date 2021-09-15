/*
const bearerToken = window.BEARER_TOKEN;

let params = {
    max_results: 100,
    "tweet.fields": "created_at",
    expansions: "author_id",
};

const options = {
    headers: {
        "User-Agent": "v2UserTweetsJS",
        authorization: `Bearer ${bearerToken}`,
    },
};

const resp = await fetch("https://api.twitter.com/2/users/2244994945/tweets");

if (resp.statusCode != 200) {
    console.log(`${resp.statusCode} ${resp.statusMessage}:\n${resp.body}`);
    return;
}
*/

window.onload = function () {
    const tweet_boxes = $(".tweet");
    let tweet_box;

    for (let i = 0; i < tweet_boxes.length; i++) {
        tweet_box = tweet_boxes[i];

        twttr.widgets.createTweet(tweet_box.id, tweet_box);
    }
};
