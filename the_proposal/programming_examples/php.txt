<?php

$parties = array(
    1=>array('votes'=>400000, 'seats'=>0),
    2=>array('votes'=>250000, 'seats'=>0),
    3=>array('votes'=>100000, 'seats'=>0),
    4=>array('votes'=>73000, 'seats'=>0),
    5=>array('votes'=>5000, 'seats'=>0)
);

$total_votes = 828000;
$seats = 5;
$quota = floor($total_votes / $seats);
$awarded = 0;

foreach ($parties as $n=>$p) {
    $awarding = floor($parties[$n]['votes'] / $quota);
    $parties[$n]['seats'] += $awarding;
    $awarded += $awarding;
}

for (; $awarded < $seats; $awarded++) {
    $n = array_reduce(array_values($parties), 
        function ($carry, $o) use ($awarded, $quota) {
            if (
                $carry['votes'] - ($carry['seats'] * $quota) > 
                $o['votes'] - ($o['seats'] * $quota)
            ) {
                return $carry;
            } else {
                return $o;
            }
        }, array('votes'=>0, 'seats'=>0)
    );
    $parties[array_search($n, $parties)]['seats'] += 1;
}

print_r($parties);

?>