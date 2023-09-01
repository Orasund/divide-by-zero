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
      , (8 - toFloat args.solved |> round |> String.fromInt)
            |> Layout.text [ Html.Attributes.style "font-size" "4em" ]
      , "more bonus levels." |> Layout.text []
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
    """
    :root {
    height: 100%;
    width: 100%;
    display: flex;
    justify-content: center;
    align-items: center;
    --rounded-edges: 1rem;
    --space: 1rem;
    --big-space: 2rem;
    font-family: Verdana, Geneva, Tahoma, sans-serif;
    --dark-color: #6b184e;
    --primary-color: #eb332b;
    --secondary-color: #f4a128;
    --light-color: #fdfcfc;
}

.light-theme {
    --background-color: var(--light-color);
    --screen-color: var(--dark-color);
}

.dark-theme {
    --background-color: var(--dark-color);
    --screen-color: var(--light-color);
}

body {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 100%;
    width: 100%;
}

#screen {
    background-color: var(--screen-color);
    border-radius: var(--rounded-edges);
    box-sizing: border-box;
    padding: var(--space);
    color: var(--background-color);
    justify-content: end;
    width: 100%;
    font-family: 'Courier New', Courier, monospace;
    font-size: 4rem;
}

.broken1 {
    position: absolute;
    left: 1px;
    top: 5px;
    clip-path: path('M0 0 L400 0 L 0 600 Z');
    width: 100%;
    animation-name: flimmer;
    animation-duration: 5s;
    animation-iteration-count: infinite;

}

.broken2 {
    position: absolute;
    left: 0px;
    top: 0px;
    clip-path: path('M400 600 L400 0 L 0 600 Z');
    width: 100%;
    filter: saturate(0.7);
}

#container {
    height: 600px;
    width: 400px;
    gap: 20px;
    border-radius: var(--rounded-edges);
    position: relative;
    left: 0px;
    padding: var(--space);
    color: var(--screen-color);
}

.info-row {
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    width: 100%;
}

.button-row {
    display: flex;
    flex-direction: row;
    flex-wrap: wrap;
    gap: 20px;
    width: 100%;
}

.column {
    display: flex;
    flex-direction: column;
    gap: var(--space);
    width: 100%;
    align-items: center;
}

button {
    background-color: var(--light-color);
    border-style: solid;
    border-color: color-mix(in lab, var(--light-color), black 10%);
    border-width: 1px 1px 0.5rem 1px;
    border-radius: var(--rounded-edges);
    padding: var(--space);
    width: 190px;
    height: 90px;
    font-size: 2rem;
    color: var(--dark-color);
}

.no-button {
    width: 190px;
    height: 90px;
}

button:hover {
    border-width: 0.125rem 1px 0.375rem 1px;
}

button:active {
    border-width: 0.5rem 1px 1px 1px;
}

button:disabled {
    border-width: 0.25rem 1px 0.25rem 1px;
    background-color: color-mix(in lab, var(--background-color), black 10%);
    border-color: color-mix(in lab, var(--background-color), black 10%);
    color: color-mix(in lab, var(--background-color), black 30%);
}

.primary {
    background-color: var(--primary-color);
    border-color: color-mix(in lab, var(--primary-color), black 10%);
    color: var(--light-color);
}

.secondary {
    background-color: var(--secondary-color);
    border-color: color-mix(in lab, var(--secondary-color), black 10%);
    color: var(--light-color);
}

#overlay {
    transition: clip-path 3s;
    /*transition-timing-function: ease-in;*/
    clip-path: circle(0%);
    background-color: color-mix(in lab, var(--dark-color), black 10%);
    height: 100%;
    width: 100%;
    color: var(--light-color);
    position: absolute;
    top: 0;
    gap: var(--big-space)
}

.game-won {
    clip-path: circle(75%) !important;
}

.shaking {
    animation-name: shake;
    animation-duration: 0.2s;
    animation-iteration-count: infinite;
}

@keyframes flimmer {

    0%,
    89%,
    100% {
        filter: saturate(1.5);
    }

    90%,
    99% {
        filter: saturate(0.7);
    }

}

@keyframes shake {
    0% {
        left: 0;
    }

    25% {
        left: -8px;
    }

    75% {
        left: 8px;
    }

    100% {
        left: 0px;
    }
}
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
