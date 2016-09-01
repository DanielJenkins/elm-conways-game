module Main exposing (..)

import Html.App
import ConwaysGame exposing (..)


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ConwaysGame.init ( 100, 100 )
        , update = ConwaysGame.update
        , view = ConwaysGame.view
        }



--Use elm.app.program
