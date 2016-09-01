module ConwaysGame exposing (Model, init, update, subscriptions, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, second)
import Matrix exposing (..)


type alias Point =
    ( Int, Int )


type LifeForce
    = Alive
    | Dead


type alias Model =
    { width : Int
    , height : Int
    , frame : Matrix LifeForce
    , ticking : Bool
    }


init : ( Int, Int ) -> Model
init ( x, y ) =
    { width = x
    , height = y
    , frame = matrix y x (\location -> Dead)
    , ticking = False
    }



-- Update


type Msg
    = FlipTicking
    | Reset
    | Tick Time


update : Msg -> Model -> Model
update msg model =
    case msg of
        FlipTicking ->
            { model | ticking = not model.ticking }

        Reset ->
            --            init
            model

        Tick newTime ->
            if model.ticking then
                nextModel model
            else
                model


nextModel : Model -> Model
nextModel model =
    { model | frame = nextFrame model.frame }


nextFrame : Matrix LifeForce -> Matrix LifeForce
nextFrame oldFrame =
    let
        f location element =
            checkForLife oldFrame location element
    in
        Matrix.mapWithLocation f oldFrame


checkForLife : Matrix LifeForce -> Location -> LifeForce -> LifeForce
checkForLife oldFrame ( x, y ) element =
    element



--    let
--        newFrame =
--            matrix Matrix.rowCount oldFrame Matrix.colCount oldFrame (\location -> False)
--    in
--        newFrame
--    if alive then
--        if fewerThanTwoLiveNeighbors oldDict ( x, y ) then
--            Nothing
--        else if moreThanThreeLiveNeighbors oldDict ( x, y ) then
--            Nothing
--        else
--            Just ( x, y )
--    else if exactlyThreeLiveNeighbors oldDict ( x, y ) then
--        Just ( x, y )
--    else
--        Nothing
--Any live cell with fewer than two live neighbours dies, as if caused by under-population.
--Any live cell with two or three live neighbours lives on to the next generation.
--Any live cell with more than three live neighbours dies, as if by over-population.
--Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.


fewerThanTwoLiveNeighbors : Dict -> Point -> Bool
fewerThanTwoLiveNeighbors oldDict ( x, y ) =
    False


moreThanThreeLiveNeighbors : Dict -> Point -> Bool
moreThanThreeLiveNeighbors oldDict ( x, y ) =
    False


exactlyThreeLiveNeighbors : Dict -> Point -> Bool
exactlyThreeLiveNeighbors oldDict ( x, y ) =
    False



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- View


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick FlipTicking ] [ text "Start/Stop" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        , div
            [ style [ ( "padding", "8px" ) ] ]
            (List.map (viewRow model) [0..model.height])
        ]


viewRow : Model -> Int -> Html msg
viewRow model row =
    div
        [ style [ ( "height", "3px" ) ] ]
        (List.map (viewCell model row) [0..model.width])


viewCell : Model -> Int -> Int -> Html msg
viewCell model row col =
    let
        color =
            case Matrix.get (loc col row) model.frame of
                Just Dead ->
                    "black"

                Just Alive ->
                    "yellow"

                Nothing ->
                    "black"
    in
        div
            [ style
                [ ( "width", "3px" )
                , ( "height", "3px" )
                , ( "background-color", color )
                , ( "display", "inline-block" )
                ]
            ]
            []
