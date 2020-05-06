{-# LANGUAGE FlexibleContexts #-} 

module Eval where

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Writer
import Control.Monad.Except

import AST

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

-- reader+either monad for interpreter functions
--   'reader' for remembering val/fun declarations in contexts
--   'either' for throwing evaluation errors
-- type Eval = ReaderT Context (Either EvalError)

-- evaluate a list of declarations
evalD :: DeclList -> Eval Context
evalD (Decls ((Val s exp):decls)) = do
  ctx <- ask
  a <- eval exp
  local (\_ -> (s,a) : ctx) $ evalD $ Decls decls


evalD (Decls ((Fun s x exp):decls)) = do
    ctx <- ask
    local (\_ -> (s, (FVal (Just s , x, exp) [])) : ctx) $ evalD $ Decls decls

evalD (Decls []) = ask

-- evaluate expression
eval :: Exp -> Eval Val

eval (Const s ) = return $ IntVal s

-- LESS THAN
eval ( Lt t1 t2 ) = do
  v1 <- eval t1
  v2 <- eval t2 
  case (v1 , v2) of
    ( IntVal c1 , IntVal c2 ) -> return $ BoolVal ( c1 < c2 )
    (_ , IntVal _) -> throwError $ NotAnInt v1
    (_ , _) -> throwError $ NotAnInt v2

-- GREATER
eval ( Gt t1 t2 ) = do
  v1 <- eval t1
  v2 <- eval t2 
  case (v1 , v2) of
    ( IntVal c1 , IntVal c2 ) -> return $ BoolVal ( c1 > c2 )
    (_ , IntVal _) -> throwError $ NotAnInt v1
    (_ , _) -> throwError $ NotAnInt v2

-- Equals
eval ( Eq t1 t2 ) = do
  v1 <- eval t1
  v2 <- eval t2 
  case (v1 , v2) of
    ( IntVal c1 , IntVal c2 ) -> return $ BoolVal ( c1 == c2 )
    (_ , IntVal _) -> throwError $ NotAnInt v1
    (_ , _) -> throwError $ NotAnInt v2

-- PLUS
eval ( Plus t1 t2 ) = do
  v1 <- eval t1
  v2 <- eval t2 
  case (v1 , v2) of
    ( IntVal c1 , IntVal c2 ) -> return $ IntVal ( c1 + c2 )
    (_ , IntVal _) -> throwError $ NotAnInt v1
    (_ , _) -> throwError $ NotAnInt v2

-- MINUS
eval ( Minus t1 t2 ) = do
  v1 <- eval t1
  v2 <- eval t2 
  case (v1 , v2) of
    ( IntVal c1 , IntVal c2 ) -> return $ IntVal ( c1 - c2 )
    (_ , IntVal _) -> throwError $ NotAnInt v1
    (_ , _) -> throwError $ NotAnInt v2

-- TIMES
eval ( Times t1 t2 ) = do
  v1 <- eval t1
  v2 <- eval t2 
  case (v1 , v2) of
    ( IntVal c1 , IntVal c2 ) -> return $ IntVal ( c1 * c2 )
    (_ , IntVal _) -> throwError $ NotAnInt v1
    (_ , _) -> throwError $ NotAnInt v2

-- DIVIDE
eval ( Div t1 t2 ) = do
  v1 <- eval t1
  v2 <- eval t2 
  case (v1 , v2) of
    ( IntVal c1 , IntVal c2 ) -> 
      if c2 /= 0 then return $ IntVal ( c1 `div` c2 )
      else throwError DivByZero
    (_ , IntVal _) -> throwError $ NotAnInt v1
    (_ , _) -> throwError $ NotAnInt v2

-- VAR REFERENCE
eval ( Var s) = do
  ctx <- ask
  case lookup s ctx of
    Just a -> return a
    Nothing -> throwError $ VariableNotFound s

-- IF STATEMENT
eval ( If t0 t1 t2 ) = do
  v <- eval t0
  case v of
    ( BoolVal b) -> if b then eval t1 else eval t2
    _ -> throwError $ NotABool v

eval (Fn t1 t2) = do
    ctx <- ask 
    return $ FVal (Nothing , t1, t2) ctx

-- APPLICATIVE (FN CALL)
eval ( App t1 t2 ) = do
  fun <- eval t1
  case fun of
    ( FVal (f , x , t0 ) ctx0 ) -> do
      arg <- eval t2
      let ctx = case f of Just name -> [( name , fun )]
                          Nothing -> []
      local (\ _ -> ctx ++ (x , arg ) : ctx0 ) $ eval t0
    _ -> throwError $ NotAFun fun

-- LET 
eval (Let dL exp) = do
  ctx <- ask
  a <- evalD dL
  local (\_ -> a ++ ctx) $ eval exp



-- run a list of declarations and print the resulting context 
runD :: DeclList -> String
runD d = y 
  where Right y = runReaderT x []
        x = do  
              a <- evalD d
              return $ "answers:\n" ++ toString a ++ "\n"
            `catchError` (\e -> return $ show e ++ "\n") 
        toString a = unlines $ map (\(x,v)-> "\t val " ++ x ++ " = " ++ show v) $ reverse a

-- run an expression and print the results
runE :: Exp -> String
runE e = y 
  where Right y = runReaderT x []
        x = do  
              a <- eval e
              return $ "answers: " ++ show a ++ "\n"
            `catchError` (\e -> return $ show e ++ "\n") 

