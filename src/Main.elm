module Main exposing (main)

import Html exposing (text)

sayHello : String -> String
sayHello name =
    "Hello " ++ name ++ "."

bottlesOf : String -> Int -> String
bottlesOf contents amount =
    Debug.toString amount ++ " bottles of " ++ contents ++ " on the wall."

main : Html.Html msg
main =
    bottlesOf "beer" 99 |>
    Html.text
