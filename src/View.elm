module View exposing (..)

import Expression exposing (Expression(..), Operator(..), Symbol(..))
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


viewInput : Game -> Symbol -> String
viewInput game symbol =
    case symbol of
        VarSymbol ->
            game.var
                |> Maybe.map Expression.toString
                |> Maybe.withDefault (Expression.symbolToString symbol)

        OpSymbol TimesOp ->
            case game.expression of
                Op TimesOp _ ->
                    Expression.symbolToString symbol ++ " → " ++ Expression.symbolToString (OpSymbol PowOp)

                _ ->
                    Expression.symbolToString symbol

        OpSymbol DividedOp ->
            case game.expression of
                Op DividedOp _ ->
                    Expression.symbolToString symbol ++ " → " ++ Expression.symbolToString (OpSymbol RootOp)

                _ ->
                    Expression.symbolToString symbol

        _ ->
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
    , [ "There are still " |> Layout.text []
      , (toFloat args.solved - 16 |> round |> String.fromInt)
            |> Layout.text [ Html.Attributes.style "font-size" "4em" ]
      , "bonus levels." |> Layout.text []
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
