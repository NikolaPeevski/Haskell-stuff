{-# LANGUAGE FlexibleContexts #-} 

module EvalTypes
        (
            Context(..),
            Val(..),
            EvalError(..),
            Eval
        ) 
        where

import AST

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Writer
import Control.Monad.Except

type Context = [(String, Val)]

data Val = IntVal Integer
         | BoolVal Bool
         | FVal (Maybe String, String, Exp) Context

instance Show Val where
    show (IntVal x) = show x
    show (BoolVal b) = show b
    show (FVal (Just f, x, e) _) =  "fn" -- "fun " ++ f ++ "..."
    show (FVal (Nothing, x, e) _) =  "(fn " ++ x ++ " => ...)"

data EvalError = VariableNotFound String
    | NotAnInt Val
    | NotABool Val
    | DivByZero
    | NotAFun Val deriving (Show)

type Eval = ReaderT Context (Either EvalError)