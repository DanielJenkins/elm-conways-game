module ConwaysGame exposing (Model, initialModel, update, subscriptions, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, millisecond)
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
    , isWon : Bool
    , score : Int
    , highScore : Int
    }


initialModel : ( Int, Int ) -> Model
initialModel ( x, y ) =
    { width = x
    , height = y
    , frame = matrix (y + 1) (x + 1) (\_ -> Dead)
    , ticking = False
    , isWon = False
    , score = 0
    , highScore = 0
    }



-- Update


type Msg
    = FlipTicking
    | Clear
    | Tick Time
    | ToggleLifeForce Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                , isWon = False
                , score = 0
              }
            , Cmd.none
            )

        ToggleLifeForce location ->
            ( { model
                | frame =
                    Matrix.set
                        location
                        (toggleLifeForce (Matrix.get location model.frame))
                        model.frame
              }
            , Cmd.none
            )

        Tick newTime ->
            if model.ticking then
                let
                    nextFrame =
                        generateNextFrame model.frame

                    nextFrameHasLive =
                        checkForWin nextFrame

                    nextScore =
                        updateScore model.score nextFrameHasLive
                in
                    ( { model
                        | isWon = nextFrameHasLive
                        , frame = nextFrame
                        , score = nextScore
                        , highScore = updateHighScore model.highScore nextScore
                        , ticking =
                            if ((Matrix.flatten nextFrame) == (Matrix.flatten model.frame)) then
                                False
                            else if nextFrameHasLive then
                                model.ticking
                            else
                                False
                      }
                    , Cmd.none
                    )
            else
                ( model
                , Cmd.none
                )


toggleLifeForce : Maybe LifeForce -> LifeForce
toggleLifeForce oldLifeForce =
    case oldLifeForce of
        Just Alive ->
            Dead

        Just Dead ->
            Alive

        Nothing ->
            Dead


generateNextFrame : Matrix LifeForce -> Matrix LifeForce
generateNextFrame frame =
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
            listOfLifeForces =
                List.map
                    (\location ->
                        if Matrix.get (location) frame == Just Alive then
                            1
                        else
                            0
                    )
                    neighborLocations
        in
            List.foldr (+) 0 listOfLifeForces


checkForWin : Matrix LifeForce -> Bool
checkForWin frame =
    let
        flatFrame =
            Matrix.flatten frame
    in
        if List.length (List.filter (\lifeForce -> lifeForce == Alive) flatFrame) > 0 then
            True
        else
            False


updateScore : Int -> Bool -> Int
updateScore score nextFrameHasLive =
    if nextFrameHasLive then
        score + 1
    else
        score


updateHighScore : Int -> Int -> Int
updateHighScore highScore nextScore =
    if nextScore > highScore then
        nextScore
    else
        highScore



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (500 * millisecond) Tick



-- View


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button
                [ onClick FlipTicking ]
                [ text "Start/Stop" ]
            , button
                [ onClick Clear ]
                [ text "New Game" ]
            , div
                []
                [ text ("This Game's Score: " ++ toString model.score ++ " -- High Score: " ++ toString model.highScore) ]
            ]
        , div
            []
            (List.map (viewRow model) [0..model.height])
        , div []
            [ text
                (if model.isWon then
                    "There's Life!"
                 else if model.score > 0 then
                    "Everything is Dead :("
                 else
                    ""
                )
            ]
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
