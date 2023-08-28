module Expression exposing (..)


type Operator
    = TimesOp
    | DividedOp
    | PowOp
    | RootOp


type Symbol
    = NumberSymbol Int
    | PointSymbol
    | OpSymbol Operator
    | VarSymbol
    | ErrSymbol


type Expression
    = Number Int
    | Dot Int
    | FloatingPoint Int Int
    | Op Operator Expression
    | Error (List Symbol)
    | DivisionByZero


invert : Operator -> Operator
invert op =
    case op of
        TimesOp ->
            DividedOp

        DividedOp ->
            TimesOp

        PowOp ->
            RootOp

        RootOp ->
            PowOp


toSymbols : Expression -> List Symbol
toSymbols exp =
    case exp of
        Number int ->
            [ NumberSymbol int ]

        FloatingPoint i1 i2 ->
            [ NumberSymbol i1
            , PointSymbol
            , NumberSymbol i2
            ]

        Dot i1 ->
            [ NumberSymbol i1
            , PointSymbol
            ]

        Op op exp2 ->
            toSymbols exp2 ++ [ OpSymbol op ]

        Error list ->
            List.reverse list ++ [ ErrSymbol ]

        DivisionByZero ->
            []


opToString : Operator -> String
opToString op =
    case op of
        TimesOp ->
            "*"

        DividedOp ->
            "/"

        PowOp ->
            "^"

        RootOp ->
            "√"


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        NumberSymbol n ->
            String.fromInt n

        OpSymbol op ->
            opToString op

        PointSymbol ->
            "."

        VarSymbol ->
            "SAVE"

        ErrSymbol ->
            "ERR"


toString : Expression -> String
toString expression =
    case expression of
        Number int ->
            String.fromInt int

        FloatingPoint i1 i2 ->
            String.fromInt i1
                ++ "."
                ++ String.fromInt i2

        Dot i1 ->
            String.fromInt i1 ++ "."

        Op operator exp ->
            toString exp
                ++ opToString operator

        Error list ->
            (list
                |> List.map symbolToString
                |> List.reverse
                |> String.concat
            )
                ++ "ERR"

        DivisionByZero ->
            "DIVIDE BY ZERO"
