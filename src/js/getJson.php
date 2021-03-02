<?php

$output = array();

function poptwice($array) {
    for ($i = 0; $i < 2; $i++) {
        array_pop($array);
    }
    return $array;
}

$path_begin = implode("/", poptwice(explode("\\", dirname(__FILE__))));

foreach (poptwice(array_reverse(scandir("$path_begin/data/" . $_GET["year"]))) as $filename) {
    $file = fopen("$path_begin/data/" . $_GET["year"] . "/$filename", "r");
    $state = explode(".", str_replace(" Of ", " of-", $filename))[0];
    $output[$state] = "";

    while (!feof($file)) {
        $output[$state] .= chop(fgets($file));
    }

    $output[$state] = json_decode($output[$state]);

    fclose($file);
}

print_r(json_encode($output));

?>