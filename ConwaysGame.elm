module ConwaysGame exposing (Model, initialModel, update, subscriptions, view)

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


initialModel : ( Int, Int ) -> Model
initialModel ( x, y ) =
    { width = x
    , height = y
    , frame = matrix (y + 1) (x + 1) (\_ -> Dead)
    , ticking = False
    }



-- Update


type Msg
    = FlipTicking
    | Clear
    | Tick Time
    | ToggleLifeForce Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        FlipTicking ->
            ( { model
                | ticking = not model.ticking
              }
            , Cmd.none
            )

        Clear ->
            ( { model
                | frame = Matrix.map (\_ -> Dead) model.frame
                , ticking = False
              }
            , Cmd.none
            )

        --TODO: Refactor this?
        ToggleLifeForce location ->
            ( { model
                | frame = Matrix.update location (\_ -> newLifeForce location model.frame) model.frame
              }
            , Cmd.none
            )

        Tick newTime ->
            if model.ticking then
                ( { model
                    | frame = nextFrame model.frame
                  }
                , Cmd.none
                )
            else
                ( model
                , Cmd.none
                )


newLifeForce : Location -> Matrix LifeForce -> LifeForce
newLifeForce location frame =
    toggleLifeForce (Matrix.get location frame)


toggleLifeForce : Maybe LifeForce -> LifeForce
toggleLifeForce oldLifeForce =
    case oldLifeForce of
        Just Alive ->
            Dead

        Just Dead ->
            Alive

        Nothing ->
            Dead


nextFrame : Matrix LifeForce -> Matrix LifeForce
nextFrame frame =
    Matrix.mapWithLocation (\location element -> checkForLife frame location element) frame


checkForLife : Matrix LifeForce -> Location -> LifeForce -> LifeForce
checkForLife frame location lifeForce =
    let
        liveNeighbors =
            countLiveNeighbors frame location
    in
        if lifeForce == Alive then
            if liveNeighbors < 2 then
                --Any live cell with fewer than two live neighbours dies
                Dead
            else if liveNeighbors > 3 then
                --Any live cell with more than three live neighbours dies
                Dead
                --Any live cell with two or three live neighbours lives on
            else
                Alive
        else if liveNeighbors == 3 then
            --Any dead cell with exactly three live neighbours becomes a live cell
            Alive
        else
            Dead


countLiveNeighbors : Matrix LifeForce -> Location -> Int
countLiveNeighbors frame ( x, y ) =
    let
        neighborLocations =
            [ ( x + 1, y + 1 )
            , ( x + 1, y - 1 )
            , ( x + 1, y )
            , ( x - 1, y + 1 )
            , ( x - 1, y - 1 )
            , ( x - 1, y )
            , ( x, y + 1 )
            , ( x, y - 1 )
            ]
    in
        let
            listOfNums =
                List.map
                    (\location ->
                        if Matrix.get (location) frame == Just Alive then
                            1
                        else
                            0
                    )
                    neighborLocations
        in
            List.foldr (+) 0 listOfNums



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
            , button [ onClick Clear ] [ text "Clear" ]
            ]
        , div
            []
            (List.map (viewRow model) [0..model.height])
        ]


viewRow : Model -> Int -> Html Msg
viewRow model row =
    div
        [ style [ ( "height", "12px" ) ] ]
        (List.map (viewCell model row) [0..model.width])


viewCell : Model -> Int -> Int -> Html Msg
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
            [ onClick (ToggleLifeForce ( col, row ))
            , style
                [ ( "width", "10px" )
                , ( "height", "10px" )
                , ( "background-color", color )
                , ( "display", "inline-block" )
                , ( "border", "1px grey solid" )
                ]
            ]
            []
