module Main exposing (main)

import Array
import Browser
import Expression exposing (Expression(..), Operator(..), Symbol(..))
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import Set exposing (Set)
import View


type alias Model =
    { game : Game
    , levelSelect : Bool
    , disabled : List Symbol
    , showOverlay : Bool
    , showDarkSide : Bool
    , solved : Set Int
    }


type Msg
    = InputPressed Symbol
    | DeletePressed
    | LevelSelectPressed
    | LoadLevelPressed Int
    | ContinuePressed


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new
      , levelSelect = False
      , disabled = []
      , showOverlay = False
      , showDarkSide = False
      , solved = Set.empty
      }
    , Cmd.none
    )


viewButtons : { won : Bool, level : Level } -> Model -> Html Msg
viewButtons args model =
    let
        noButton =
            Html.div [ Html.Attributes.class "no-button" ] []

        placeholder =
            Layout.textButton
                [ Html.Attributes.disabled True
                ]
                { label = ""
                , onPress = Nothing
                }
    in
    (if model.levelSelect then
        let
            min =
                if model.showDarkSide then
                    9

                else
                    1
        in
        List.range min (min + 7)
            |> List.map
                (\i ->
                    Layout.textButton
                        (if i == model.game.level then
                            [ Html.Attributes.class "primary" ]

                         else if model.solved |> Set.member i then
                            [ Html.Attributes.class "secondary" ]

                         else
                            []
                        )
                        { label = String.fromInt i
                        , onPress =
                            LoadLevelPressed i
                                |> Just
                        }
                )

     else
        [ Layout.textButton
            [ Html.Attributes.class "secondary"
            , Html.Attributes.disabled args.won
            ]
            { label = "LVL"
            , onPress =
                LevelSelectPressed
                    |> Just
            }
        , Layout.textButton
            [ Html.Attributes.class "primary"
            , Html.Attributes.disabled args.won
            ]
            { label = "DEL"
            , onPress =
                DeletePressed
                    |> Just
            }
        ]
            ++ (args.level.inputs
                    |> List.map
                        (\input ->
                            Layout.textButton
                                [ Html.Attributes.disabled (List.member input model.disabled || args.won) ]
                                { label = View.viewInput model.game input
                                , onPress =
                                    input
                                        |> InputPressed
                                        |> Just
                                }
                        )
               )
            ++ (placeholder
                    |> List.repeat (4 - List.length args.level.inputs)
               )
            ++ [ Layout.textButton
                    [ Html.Attributes.class "secondary"
                    , Html.Attributes.disabled
                        ((args.level.withVar && List.member VarSymbol model.disabled)
                            || (not args.level.withVar && model.game.var == Nothing)
                            || args.won
                        )
                    ]
                    { label = View.viewInput model.game VarSymbol
                    , onPress =
                        VarSymbol
                            |> InputPressed
                            |> Just
                    }
               , Layout.textButton
                    [ Html.Attributes.class "secondary"
                    , Html.Attributes.disabled (not args.won)
                    ]
                    { label = "NEXT"
                    , onPress =
                        LoadLevelPressed (model.game.level + 1)
                            |> Just
                    }
               ]
    )
        |> Html.div [ Html.Attributes.class "button-row" ]


view : Model -> Html Msg
view model =
    let
        level =
            Array.get model.game.level Level.levels
                |> Maybe.withDefault Level.errorLevel

        gameWon =
            model.showOverlay

        levelWon =
            model.game.expression == level.goal

        gameBroken =
            case model.game.expression of
                Error _ ->
                    True

                _ ->
                    False
    in
    [ View.stylesheet
    , [ (if model.levelSelect == False then
            if model.game.expression == DivisionByZero then
                "COMPUTING"

            else
                model.game.expression
                    |> Expression.toString

         else
            "LEVEL = "
        )
            |> Html.text
            |> Layout.el [ Html.Attributes.id "screen" ]
      , [ "Level " ++ String.fromInt model.game.level |> Layout.text []
        , "Goal: " ++ Expression.toString level.goal |> Layout.text [ Html.Attributes.style "font-weight" "bold" ]
        ]
            |> Html.div [ Html.Attributes.class "info-row" ]
      , if gameBroken then
            [ [ viewButtons
                    { level = level
                    , won = levelWon
                    }
                    model
              ]
                |> Html.div [ Html.Attributes.class "broken1" ]
            , [ viewButtons
                    { level = level
                    , won = levelWon
                    }
                    model
              ]
                |> Html.div [ Html.Attributes.class "broken2" ]
            ]
                |> Html.div [ Html.Attributes.style "position" "relative" ]

        else
            viewButtons
                { level = level
                , won = levelWon
                }
                model
      ]
        |> Layout.column
            (Html.Attributes.id "container"
                :: (if gameWon then
                        [ Html.Attributes.class "shaking" ]

                    else
                        []
                   )
            )
    , View.overlay
        [ Html.Attributes.class "light-theme" ]
        { gameWon = gameWon
        , onContinue = ContinuePressed
        , solved = Set.size model.solved
        }
    ]
        |> Layout.column
            ([ Html.Attributes.style "width" "100%"
             , Html.Attributes.style "height" "100%"
             , Html.Attributes.style "position" "relative"
             , Html.Attributes.class
                (if model.showDarkSide then
                    "dark-theme"

                 else
                    "light-theme"
                )
             , Html.Attributes.style "background-color" "var(--background-color)"
             ]
                ++ Layout.centered
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ContinuePressed ->
            ( { model
                | showOverlay = False
                , showDarkSide = not model.showDarkSide
                , disabled = []
                , game = model.game |> Game.loadLevel (model.game.level + 1)
              }
            , Cmd.none
            )

        InputPressed input ->
            ( model.game
                |> Game.addSymbol input
                |> (\game ->
                        { model
                            | game = game
                            , disabled =
                                case game.expression of
                                    Error _ ->
                                        []

                                    _ ->
                                        if input == VarSymbol && model.game.var == Nothing then
                                            model.disabled

                                        else
                                            input :: model.disabled
                            , showOverlay = game.expression == DivisionByZero
                            , solved =
                                if
                                    Level.levels
                                        |> Array.get model.game.level
                                        |> Maybe.withDefault Level.errorLevel
                                        |> .goal
                                        |> (==) game.expression
                                then
                                    model.solved |> Set.insert model.game.level

                                else
                                    model.solved
                        }
                   )
            , Cmd.none
            )

        DeletePressed ->
            ( model.game
                |> Game.deleteInput
                |> (\game ->
                        { model
                            | game = game
                            , disabled =
                                case game.expression of
                                    Error _ ->
                                        []

                                    _ ->
                                        if game.expression == Number 0 then
                                            []

                                        else
                                            model.disabled
                            , showOverlay = game.expression == DivisionByZero
                            , solved =
                                if
                                    Level.levels
                                        |> Array.get model.game.level
                                        |> Maybe.withDefault Level.errorLevel
                                        |> .goal
                                        |> (==) game.expression
                                then
                                    model.solved |> Set.insert model.game.level

                                else
                                    model.solved
                        }
                   )
            , Cmd.none
            )

        LevelSelectPressed ->
            if model.levelSelect == False then
                ( { model | levelSelect = True }
                , Cmd.none
                )

            else
                ( { model | levelSelect = False }
                , Cmd.none
                )

        LoadLevelPressed level ->
            ( { model
                | levelSelect = False
                , game = model.game |> Game.loadLevel level
                , disabled = []
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
