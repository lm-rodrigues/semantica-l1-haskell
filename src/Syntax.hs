module Syntax where

import           Environment (Environment)


{-| Provides the basic elements of Syntax of Typed L1.
-}
data Type
    = TINT
    | TBOOL
    | TFN FunctionType
    deriving (Eq, Show)


type FunctionType =
    ( Type, Type )


data BinaryOperator
    = SUM
    | DIFF
    | MULT
    | DIV
    | LEQ
    | LT
    | GEQ
    | GT
    | EQ
    | NEQ
    | AND
    | OR
    deriving (Eq, Show)

data UnaryOperator
    = NOT
    deriving (Eq, Show)

data Value
    = VNUM Int
    | VBOOL Bool
    | Closure Variable Expression (Environment Variable Value)
    | RClosure Variable Variable Expression (Environment Variable Value)
    deriving (Eq, Show)

type Variable =
    String


type Function =
    ( Variable, Type, Expression )


data Expression
    = ENUM Int
    | EBOOL Bool
    | UOP UnaryOperator Expression
    | BOP Expression BinaryOperator Expression
    | VAR Variable
    | IF Expression Expression Expression
    | APP Expression Expression
    | FN Function
    | LET Function Expression
    | LETREC Variable FunctionType Function Expression
    deriving (Eq, Show)
