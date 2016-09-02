module Main exposing (..)

import Html.App
import ConwaysGame exposing (..)


main : Program Never
main =
    Html.App.program
        { init = ( ConwaysGame.initialModel ( 35, 35 ), Cmd.none )
        , subscriptions = ConwaysGame.subscriptions
        , update = ConwaysGame.update
        , view = ConwaysGame.view
        }
