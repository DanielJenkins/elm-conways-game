module Main exposing (..)

import Html.App
import ConwaysGame exposing (..)


main : Program Never
main =
    Html.App.program
        { init = ( ConwaysGame.initialModel ( 20, 20 ), Cmd.none )
        , subscriptions = ConwaysGame.subscriptions
        , update = ConwaysGame.update
        , view = ConwaysGame.view
        }
