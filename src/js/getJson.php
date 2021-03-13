<?php

$output = array();

function poptwice($array) {
    for ($i = 0; $i < 2; $i++) {
        array_pop($array);
    }
    return $array;
}

if (isset($_GET["string"]))
    $replace = " of ";
else 
    $replace = " of-";

$path_begin = implode("/", poptwice(explode("\\", dirname(__FILE__))));

foreach (poptwice(array_reverse(scandir("$path_begin/data/" . $_GET["year"]))) as $filename) {
    $file = fopen("$path_begin/data/" . $_GET["year"] . "/$filename", "r");
    $state = explode(".", str_replace(" Of ", $replace, $filename))[0];
    $output[$state] = "";

    while (!feof($file)) {
        $output[$state] .= chop(fgets($file));
    }

    if (isset($_GET["string"])) 
        $output[$state] = $output[$state];
    else 
        $output[$state] = json_decode($output[$state]);

    fclose($file);
}

print_r(json_encode($output));

?>