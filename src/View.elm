module View exposing (..)

import Expression exposing (Operator(..), Symbol(..))
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


viewInput : Game -> Symbol -> String
viewInput game symbol =
    if symbol == VarSymbol then
        game.var
            |> Maybe.map Expression.toString
            |> Maybe.withDefault (Expression.symbolToString symbol)

    else
        Expression.symbolToString symbol


overlay :
    List (Attribute msg)
    ->
        { gameWon : Bool
        , onContinue : msg
        , solved : Int
        }
    -> Html msg
overlay attrs args =
    [ "Divide by Zero" |> Layout.text [ Html.Attributes.style "font-size" "4em", Html.Attributes.style "text-align" "center" ]
    , "A game by Lucas Payr" |> Layout.text [ Html.Attributes.style "font-size" "1.5em" ]
    , [ "You solved" |> Layout.text []
      , (toFloat args.solved * 100 / 16 |> round |> String.fromInt)
            ++ "%"
            |> Layout.text [ Html.Attributes.style "font-size" "4em" ]
      , "of all levels." |> Layout.text []
      ]
        |> Layout.column [ Html.Attributes.class "column" ]
    , Layout.textButton [ Html.Attributes.style "width" "initial" ]
        { label = "Continue"
        , onPress = Just args.onContinue
        }
    ]
        |> Layout.column
            (Html.Attributes.id "overlay"
                :: (if args.gameWon then
                        [ Html.Attributes.class "game-won" ]

                    else
                        []
                   )
                ++ Layout.centered
                ++ attrs
            )


stylesheet : Html msg
stylesheet =
    Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.href "style.css"
        ]
        []
