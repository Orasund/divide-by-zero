module Level exposing (..)

import Array exposing (Array)
import Expression exposing (Expression(..), Operator(..), Symbol(..))


type alias Level =
    { inputs : List Symbol
    , goal : Expression
    , withVar : Bool
    }


errorLevel : Level
errorLevel =
    { inputs =
        [ PointSymbol
        , ErrSymbol
        ]
    , withVar = True
    , goal = DivisionByZero
    }


levels : Array Level
levels =
    errorLevel
        :: baseGame
        ++ darkSide
        |> Array.fromList


baseGame : List Level
baseGame =
    timesTrack
        ++ divideTrack
        ++ varTrack


darkSide : List Level
darkSide =
    [ { inputs =
            [ NumberSymbol 1
            , OpSymbol DividedOp
            ]
      , withVar = True
      , goal = FloatingPoint 0 1
      }
    , { inputs =
            [ OpSymbol TimesOp
            , NumberSymbol 0
            ]
      , withVar = True
      , goal = Number 1
      }
    ]
        ++ errorTrack
        ++ [ {--{ inputs =
                [ NumberSymbol 2
                , NumberSymbol 3
                , OpSymbol TimesOp
                , OpSymbol DividedOp
                ]
             , withVar = True
             , goal = Number 666
             }
           , --}
             { inputs = [ OpSymbol DividedOp ]
             , withVar = True
             , goal = Number 6
             }
           ]


errorTrack : List Level
errorTrack =
    [ { inputs =
            [ OpSymbol TimesOp
            , OpSymbol DividedOp
            ]
      , withVar = True
      , goal = Error []
      }
    , { inputs =
            [ OpSymbol TimesOp
            , OpSymbol DividedOp
            , NumberSymbol 2
            ]
      , withVar = True
      , goal = Number 222
      }
    , { inputs =
            [ OpSymbol TimesOp
            , OpSymbol DividedOp
            , NumberSymbol 3
            ]
      , withVar = True
      , goal = Number 300
      }
    , { inputs =
            [ OpSymbol TimesOp
            , OpSymbol DividedOp
            ]
      , withVar = True
      , goal = Number 1
      }
    , { inputs =
            [ OpSymbol TimesOp
            , OpSymbol DividedOp
            ]
      , withVar = True
      , goal = FloatingPoint 0 0
      }
    ]


varTrack : List Level
varTrack =
    [ { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = True
      , goal = Number 212
      }
    , { inputs =
            [ NumberSymbol 2
            , OpSymbol TimesOp
            ]
      , withVar = True
      , goal = Number 4
      }
    , { inputs =
            [ NumberSymbol 4
            ]
      , withVar = True
      , goal = Number 40
      }
    , { inputs =
            [ OpSymbol TimesOp
            , OpSymbol DividedOp
            ]
      , withVar = True
      , goal = DivisionByZero
      }
    ]


timesTrack : List Level
timesTrack =
    [ { inputs =
            [ NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = False
      , goal = Number 12
      }
    , { inputs =
            [ NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = False
      , goal = Number 1
      }
    ]


divideTrack : List Level
divideTrack =
    [ { inputs =
            [ NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol DividedOp
            ]
      , withVar = False
      , goal = Number 1
      }
    , { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol DividedOp
            ]
      , withVar = False
      , goal = Number 12
      }
    ]


setVarToOp : Level
setVarToOp =
    { inputs =
        [ NumberSymbol 1
        , NumberSymbol 2
        , NumberSymbol 3
        , OpSymbol TimesOp
        , VarSymbol
        ]
    , withVar = False
    , goal = Number 18
    }
