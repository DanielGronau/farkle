module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Modal as Modal
import Browser
import Die exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Util



-- ██████   ██████            ███
--░░██████ ██████            ░░░
-- ░███░█████░███   ██████   ████  ████████
-- ░███░░███ ░███  ░░░░░███ ░░███ ░░███░░███
-- ░███ ░░░  ░███   ███████  ░███  ░███ ░███
-- ░███      ░███  ███░░███  ░███  ░███ ░███
-- █████     █████░░████████ █████ ████ █████
--░░░░░     ░░░░░  ░░░░░░░░ ░░░░░ ░░░░ ░░░░░


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- ██████   ██████              █████          ████
--░░██████ ██████              ░░███          ░░███
-- ░███░█████░███   ██████   ███████   ██████  ░███
-- ░███░░███ ░███  ███░░███ ███░░███  ███░░███ ░███
-- ░███ ░░░  ░███ ░███ ░███░███ ░███ ░███████  ░███
-- ░███      ░███ ░███ ░███░███ ░███ ░███░░░   ░███
-- █████     █████░░██████ ░░████████░░██████  █████
--░░░░░     ░░░░░  ░░░░░░   ░░░░░░░░  ░░░░░░  ░░░░░


type alias Points =
    Int


type alias Model =
    { active : Points
    , inactive : Points
    , onTable : Points
    , rounds : Int
    , dice : Dice
    , dialog : Maybe ( String, Msg )
    }



-- █████             ███   █████
--░░███             ░░░   ░░███
-- ░███  ████████   ████  ███████
-- ░███ ░░███░░███ ░░███ ░░░███░
-- ░███  ░███ ░███  ░███   ░███
-- ░███  ░███ ░███  ░███   ░███ ███
-- █████ ████ █████ █████  ░░█████
--░░░░░ ░░░░ ░░░░░ ░░░░░    ░░░░░


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, rollDice 6 )


initModel : Model
initModel =
    { active = 0
    , inactive = 0
    , onTable = 0
    , rounds = 0
    , dice = []
    , dialog = Nothing
    }



-- █████  █████               █████            █████
--░░███  ░░███               ░░███            ░░███
-- ░███   ░███  ████████   ███████   ██████   ███████    ██████
-- ░███   ░███ ░░███░░███ ███░░███  ░░░░░███ ░░░███░    ███░░███
-- ░███   ░███  ░███ ░███░███ ░███   ███████   ░███    ░███████
-- ░███   ░███  ░███ ░███░███ ░███  ███░░███   ░███ ███░███░░░
-- ░░████████   ░███████ ░░████████░░████████  ░░█████ ░░██████
--  ░░░░░░░░    ░███░░░   ░░░░░░░░  ░░░░░░░░    ░░░░░   ░░░░░░
--              ░███
--              █████
--             ░░░░░


type Msg
    = NewGame
    | DiceRoll (List Int)
    | ToggleDie Int
    | Roll
    | Bank
    | Farkled
    | GiveUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( roll model
            , rollDice <| remainingDice model
            )

        Bank ->
            if win model then
                ( { model | dialog = Just ( "You won", NewGame ) }, Cmd.none )

            else
                ( bank model, rollDice 6 )

        DiceRoll faces ->
            ( diceRoll model faces
            , Cmd.none
            )

        ToggleDie index ->
            ( toggleDice model index
            , Cmd.none
            )

        NewGame ->
            init ()

        Farkled ->
            ( farkled model
            , rollDice 6
            )

        GiveUp ->
            ( { model | dialog = Just ( "Starting a new game", NewGame ) }
            , Cmd.none
            )


farkled : Model -> Model
farkled model =
    { active = model.inactive
    , inactive = model.active
    , onTable = 0
    , rounds = model.rounds + 1
    , dice = []
    , dialog = Nothing
    }


roll : Model -> Model
roll model =
    { model
        | onTable = model.onTable + newOnTable model
        , dialog = Nothing
    }


diceRoll : Model -> List Int -> Model
diceRoll model faces =
    { model
        | dice = createDice faces
        , dialog = Util.toMaybe (hasFarkled <| List.sort faces) ( "You have farkled", Farkled )
    }


remainingDice : Model -> Int
remainingDice model =
    let
        remaining =
            model.dice
                |> List.filter (not << isMarked)
                |> List.length
    in
    if remaining == 0 then
        6

    else
        remaining


win : Model -> Bool
win model =
    model.active + model.onTable + newOnTable model >= 4000


bank : Model -> Model
bank model =
    { active = model.inactive
    , inactive = model.active + model.onTable + newOnTable model
    , onTable = 0
    , rounds = model.rounds + 1
    , dice = []
    , dialog = Nothing
    }


rollDice : Int -> Cmd Msg
rollDice size =
    Random.int 1 6
        |> Random.list size
        |> Random.generate DiceRoll


toggleDice : Model -> Int -> Model
toggleDice model index =
    { model
        | dice = Die.toggle index model.dice
        , dialog = Nothing
    }



--  █████████             █████
-- ███░░░░░███           ░░███
--░███    ░░░  █████ ████ ░███████   █████
--░░█████████ ░░███ ░███  ░███░░███ ███░░
-- ░░░░░░░░███ ░███ ░███  ░███ ░███░░█████
-- ███    ░███ ░███ ░███  ░███ ░███ ░░░░███
--░░█████████  ░░████████ ████████  ██████
-- ░░░░░░░░░    ░░░░░░░░ ░░░░░░░░  ░░░░░░
--
--
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- █████   █████  ███
--░░███   ░░███  ░░░
-- ░███    ░███  ████   ██████  █████ ███ █████
-- ░███    ░███ ░░███  ███░░███░░███ ░███░░███
-- ░░███   ███   ░███ ░███████  ░███ ░███ ░███
--  ░░░█████░    ░███ ░███░░░   ░░███████████
--    ░░███      █████░░██████   ░░████░████
--     ░░░      ░░░░░  ░░░░░░     ░░░░ ░░░░


view : Model -> Html Msg
view model =
    Grid.container [ style "margin-top" "20px" ]
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ h1 [ class "text-center bg-secondary text-white" ] [ text "Farkle" ] ]
            ]
        , Grid.row []
            [ Grid.col []
                [ h3 [ playerFormat 0 model ]
                    [ text <| "Player 1: " ++ String.fromInt (firstPlayerPoints model) ++ " points" ]
                ]
            , Grid.col []
                [ h3 [ playerFormat 1 model ]
                    [ text <| "Player 2: " ++ String.fromInt (secondPlayerPoints model) ++ " points" ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ h4 []
                    [ text <|
                        activePlayerText model
                            ++ " has "
                            ++ String.fromInt model.onTable
                            ++ " points on the table and "
                            ++ String.fromInt (newOnTable model)
                            ++ " points are selected."
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                (List.indexedMap faceToImgTag model.dice)
            ]
        , Grid.row []
            [ Grid.col []
                [ bootstrapButton Roll (noValidSelection model) "Roll" ]
            , Grid.col []
                [ bootstrapButton Bank (noValidSelection model) "Bank" ]
            , Grid.col []
                [ bootstrapButton GiveUp False "Give up / Restart" ]
            ]
        , Grid.row []
            [ Grid.col []
                [ a [ href "https://kingdom-come-deliverance.vidyawiki.com/Farkle", target "_blank" ]
                    [ text "Using the rules from Kingdom Come Deliverance" ]
                ]
            ]
        , model.dialog
            |> Maybe.map (\( textMessage, msg ) -> modal textMessage msg)
            |> Maybe.withDefault (div [] [])
        ]


playerFormat : Int -> Model -> Attribute Msg
playerFormat m model =
    if m == modBy 2 model.rounds then
        class "bg-info text-white"

    else
        class "bg-light text-dark"


bootstrapButton : Msg -> Bool -> String -> Html Msg
bootstrapButton msg isDisabled caption =
    button [ class "btn btn-dark btn-block btn-lg", onClick msg, disabled isDisabled ] [ text caption ]


modal : String -> Msg -> Html Msg
modal textMessage msg =
    Modal.config msg
        |> Modal.small
        |> Modal.h4 [] [ text textMessage ]
        |> Modal.view Modal.shown


noValidSelection : Model -> Bool
noValidSelection model =
    let
        marked =
            markedDice model.dice
    in
    not (Util.isJust <| applyCombis marked) || List.isEmpty marked


activePlayerText : Model -> String
activePlayerText model =
    if modBy 2 model.rounds == 0 then
        "Player 1"

    else
        "Player 2"


firstPlayerPoints : Model -> Points
firstPlayerPoints model =
    if modBy 2 model.rounds == 0 then
        model.active

    else
        model.inactive


secondPlayerPoints : Model -> Points
secondPlayerPoints model =
    if modBy 2 model.rounds == 1 then
        model.active

    else
        model.inactive


faceToImgTag : Int -> Die -> Html Msg
faceToImgTag index die =
    img [ src <| "assets/" ++ imageFile die, style "padding" "5px", onClick <| ToggleDie <| index ] []


imageFile : Die -> String
imageFile die =
    (case isMarked die of
        True ->
            "b"

        False ->
            "w"
    )
        ++ String.fromInt (dieFace die)
        ++ ".png"


newOnTable : Model -> Int
newOnTable model =
    model.dice
        |> markedDice
        |> applyCombis
        |> Maybe.withDefault 0
