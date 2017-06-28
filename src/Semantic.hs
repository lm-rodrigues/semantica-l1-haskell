module Semantic where

import           Environment
import           Syntax      as S


type Env =
    Environment Variable Value


internalEval :: Env -> Expression -> Value
internalEval environment expression =
    case expression of
        ENUM number ->
            VNUM number

        EBOOL boolean ->
            VBOOL boolean

        UOP operator expressionOne ->
            unaryOperationEval environment operator expressionOne

        BOP expressionOne operator expressionTwo ->
            binaryOperationEval environment expressionOne operator expressionTwo

        VAR variable ->
            envLookup variable environment

        IF condition thenExpression elseExpression ->
            ifEval environment condition thenExpression elseExpression

        APP function argument ->
            appEval environment function argument

        FN ( variable, _, expression ) ->
            Closure variable expression environment

        LET function expression ->
            letEval environment function expression

        LETREC variable _ function expression ->
            letRecEval environment variable function expression


unaryOperationEval :: Env -> UnaryOperator -> Expression -> Value
unaryOperationEval environment operator expressionOne =
    let
        expressionOneEval =
            internalEval environment expressionOne
    in
        case ( operator, expressionOneEval ) of
            ( NOT, (VBOOL valueOne) ) ->
                VBOOL (not valueOne)


binaryOperationEval :: Env -> Expression -> BinaryOperator -> Expression -> Value
binaryOperationEval environment expressionOne operator expressionTwo =
    let
        expressionOneEval =
            internalEval environment expressionOne

        expressionTwoEval =
            internalEval environment expressionTwo
    in
        case ( expressionOneEval, expressionTwoEval ) of
            ((VNUM valueOne), (VNUM valueTwo) ) ->
                case operator of
                    SUM ->
                        VNUM (valueOne + valueTwo)

                    DIFF ->
                        VNUM (valueOne - valueTwo)

                    MULT ->
                        VNUM (valueOne * valueTwo)

                    DIV ->
                        VNUM (valueOne `div` valueTwo)

                    S.LT ->
                        VBOOL (valueOne < valueTwo)

                    S.LEQ ->
                        VBOOL (valueOne <= valueTwo)

                    S.GT ->
                        VBOOL (valueOne > valueTwo)

                    S.GEQ ->
                        VBOOL (valueOne >= valueTwo)

                    S.EQ ->
                        VBOOL (valueOne == valueTwo)

                    S.NEQ ->
                        VBOOL (valueOne /= valueTwo)


            ((VBOOL valueOne), (VBOOL valueTwo) ) ->
                case operator of
                    S.EQ ->
                         VBOOL (valueOne == valueTwo)

                    S.NEQ ->
                         VBOOL (valueOne /= valueTwo)

                    S.AND ->
                         VBOOL (valueOne && valueTwo)

                    S.OR ->
                         VBOOL (valueOne || valueTwo)


ifEval :: Env -> Expression -> Expression -> Expression -> Value
ifEval environment condition thenCase elseCase =
    let
        conditionEval =
            internalEval environment condition

        thenCaseEval =
            internalEval environment thenCase

        elseCaseEval =
            internalEval environment elseCase
    in
        case conditionEval of
            (VBOOL True) ->
                thenCaseEval

            (VBOOL False) ->
                elseCaseEval


appEval :: Env -> Expression -> Expression -> Value
appEval environment function argument =
    let
        functionEval =
            internalEval environment function

        argumentEval =
            internalEval environment argument
    in
        case ( functionEval, argumentEval ) of
            ((Closure variable subExpression fnEnvironment), value ) ->
                internalEval
                    (insertEnv variable value fnEnvironment)
                    subExpression

            ((RClosure recVariable fnVariable subExpression fnEnvironment), value ) ->
                let
                    recClosure =
                        RClosure recVariable fnVariable subExpression fnEnvironment
                in
                    internalEval
                    (insertEnv fnVariable value $
                     insertEnv recVariable recClosure fnEnvironment )
                    subExpression


letEval :: Env -> Function -> Expression -> Value
letEval environment ( variable, _, subExpression ) expression =
    let
        subExpressionEval =
            internalEval environment subExpression
    in
        case subExpressionEval of
            value ->
                internalEval
                    (insertEnv variable value environment)
                    expression


letRecEval :: Env -> Variable -> Function -> Expression -> Value
letRecEval environment recVariable ( fnVariable, _, subExpression ) expression =
    let
        recursiveEnvironment =
            insertEnv
                recVariable
                (RClosure recVariable fnVariable subExpression environment)
                environment
    in
        internalEval recursiveEnvironment expression
