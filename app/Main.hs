{-
 -  <DeclList> ::= { (<FunDecl> | <ValDecl>) }
 -
 -  <FunDecl> ::= 'fun' <Ident> <Ident> '=' <Exp> 
 -
 -  <ValDecl> ::= 'val' <Ident> '=' <Exp> 
 -
 -  <Expr> ::= <Comp> 
 -           | 'if' <Expr> 'then' <Expr> 'else' <Expr>
 -           | 'let' <DeclList> 'in' <Expr> 'end'
 -           | 'fn' <Ident> '=>' <Expr>
 -
 -  <Comp> ::= <Plus> { ('>' | '=' | '<') <Plus> }
 -
 -  <Plus> ::= <Mult> { ('+' | '-') <Mult> }
 -
 -  <Mult> ::= <App>  { ('*' | '/') <App> }
 -
 -  <App>  ::= <Fact> { <Fact> }
 -
 -  <Fact> ::= '(' <Expr> ')' 
 -          |  <Integer>     
 -          |  <Identifier>
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE FlexibleContexts #-}


-- import Hw1
-- import Hw2
-- import Hw3
-- import Hw4
-- import Hw5
-- import Hw6
-- import Hw7
import Data.Time.Clock
import Data.Complex


import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim (unexpected)
import Data.Either
import qualified Text.Parsec.Token as Token

-- Abstract Syntax Tree ----------------------------------------------------------------------------------

-- function/variable declaration
data Decl = Fun String String Exp -- fun f x = e;
          | Val String Exp        -- val x = e;

-- expressions
data Exp = Lt Exp Exp     -- e1 < e2
        | Gt Exp Exp     -- e1 > e2
        | Eq Exp Exp     -- e1 = e2
        | Plus Exp Exp   -- e1 + e2
        | Minus Exp Exp  -- e1 - e2
        | Times Exp Exp  -- e1 * e2
        | Div Exp Exp    -- e1 div e2
        | Var String     -- x
        | If Exp Exp Exp -- if e0 then e1 else e2
        | Fn String Exp  -- fn x => e
        | Let [Decl] Exp -- let val x = e0; fun f = e1; in e2 end
        | App Exp Exp    -- e1 e2
        | Const Integer  -- n

instance Show Decl where
  show (Fun f x e) = "fun " ++ f ++ " " ++ x ++ " = " ++ show e 
  show (Val x e) = "val " ++ x ++ " = " ++ show e 

instance Show Exp where
  show (Const x) = show x
  show (Plus t1 t2) = showBop t1 "+" t2
  show (Times t1 t2) = showBop t1 "*" t2
  show (Minus t1 t2) = showBop t1 "-" t2
  show (Div t1 t2) = showBop t1 "/" t2
  show (Lt t1 t2) = showBop t1 "<" t2
  show (Gt t1 t2) = showBop t1 ">" t2
  show (Eq t1 t2) = showBop t1 "=" t2
  show (If t0 t1 t2) = "if " ++ show t0 ++ " then " ++ show t1 ++ " else " ++ show t2
  show (Var s) = s
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Fn x t) = "(fn" ++ x ++ " => " ++ show t ++ ")"
  show (Let decls e) = "(let " ++ show decls ++ " in " ++ show e ++ " end)"

showBop t1 op t2 = "(" ++ show t1 ++ " " ++ op ++ " " ++ show t2 ++ ")"


-- Parser definitions ----------------------------------------------------------------------------------

type Parser = Parsec [Char] ()

-- Runner

-- print AST or error
run :: Show a => Parser a -> String -> IO()
run p input = putStrLn $ case parse p "" input of
      Left error -> show error
      Right x    -> show x

-- return Either AST or parse error 
runParser :: Parser a -> String -> Either ParseError a
runParser p input = parse p "" input 

-- Lexer

lexeme p = do { x <- p; skipMany space <?> ""; return x }  -- get rid of trailing spaces

symbol name = lexeme $ string name                         -- parse string and then remove spaces

ident :: Parser String                                     -- raw identifier
ident = (lexeme $ many1 letter) <?> "identifier"

integer :: Parser Integer                                  -- parse integer and return numeric value
integer = (read <$> (lexeme $ many1 digit)) <?> "integer"

keyword :: String -> Parser ()                             -- make keyword parser with backtrack
keyword name = (try $ symbol name) >> return ()

if_ = keyword "if"                                         -- keyword parsers
then_ = keyword "then"
else_ = keyword "else"
fn_ = keyword "fn"
fun_ = keyword "fun"
let_ = keyword "let"
in_ = keyword "in"
end_ = keyword "end"
val_ = keyword "val"
assign_ = keyword "="
gt_ = keyword ">"
lt_ = keyword "<"
eq_ = keyword "="
pl_ = keyword "+"
mn_ = keyword "-"
tm_ = keyword "*"
dv_ = keyword "/"
fatArrow_ = keyword "=>"
sCol_ = keyword ";"

identifier :: Parser String                                -- identifier that won't collide with keywords
identifier = try $ do 
    name <- ident 
    if name `elem` reserved 
      then unexpected ("reserved word " ++ name) -- throw parse error 
      else return name 
  where 
    reserved = ["if", "then", "else", "fn", "fun", "let", "in", "end", "val"]

-- Parser for the (phrase) grammar

prog :: Parser [Decl]         
prog = skipMany space >> declList                              -- skip leading spaces

declList :: Parser [Decl]                                      -- parse a list of function or variable declarations
declList = many $ valDecl <|> funDecl


funDecl :: Parser Decl
funDecl = do
  fun_
  name <- identifier
  param <- identifier
  assign_
  t1 <- parseExpression
  return (Fun name param t1)

valDecl :: Parser Decl
valDecl = do
  val_
  name <- identifier
  assign_
  t1 <- parseExpression
  return (Val name t1)

parseExpression :: Parser Exp
parseExpression = formula

parseInt :: Parser Exp
parseInt = Const `fmap` integer

formula :: Parser Exp
formula = buildExpressionParser [[add,sub]] juxta <?> "formula"
  where add = Infix (pl_ >> return addExpr) AssocLeft
        sub = Infix (mn_ >> return subExpr) AssocLeft
        -- mulOp = Infix (tm_ >> return parseTm) AssocLeft

juxta :: Parser Exp
juxta = (foldl1 App) `fmap` (many1 atom)

atom :: Parser Exp
atom = parseVar <|> parseInt <|> parseExpression <?> "atom"

addExpr :: Exp -> Exp -> Exp
addExpr = Plus

subExpr :: Exp -> Exp -> Exp
subExpr = Minus


parseIf :: Parser Exp
parseIf = do
  if_
  cond <- parseExpression
  then_
  true <- parseExpression
  else_
  false <- parseExpression
  return (If cond true false)

parseCond :: Parser Exp
parseCond = parseLt <|> parseGt

parseLt :: Parser Exp
parseLt = do
  exp1 <- parseExpression
  lt_
  exp2 <- parseExpression
  return (Lt exp1 exp2)

parseGt :: Parser Exp
parseGt = do
  exp1 <- parseExpression
  gt_
  exp2 <- parseExpression
  return (Gt exp1 exp2)

parseEq :: Parser Exp
parseEq = do
  exp1 <- parseExpression
  eq_
  exp2 <- parseExpression
  return (Eq exp1 exp2)

parsePl :: Parser Exp
parsePl = do
  exp1 <- parseExpression
  pl_
  exp2 <- parseExpression
  return (Plus (Const 5) (Const 6))

parseMn :: Parser Exp
parseMn = do
  exp1 <- parseExpression
  mn_
  exp2 <- parseExpression
  return (Plus exp1 exp2)

parseApp :: Parser Exp
parseApp = do
  exp1 <- parseExpression
  exp2 <- parseExpression
  return (App exp1 exp2)

parseVar :: Parser Exp
parseVar = Var `fmap` identifier



  


a = "name"


-- TODO






-- Test code ----------------------------------------------------------------------------------

main :: IO ()
main = do

        let fact = "fun fact x = if x < 1 then 1 else x * fact (x-1)\n"
        let it = "val it = let val y = 10 in fact y end"
        
        let d = (fact ++ it)
        let test =  "val x = x\n"
        let test2 =  "fun fact x = 5\n"
        let test3 = "if x > 1 then 1 else -1"
        let test4 = "fun fact x = y"
        let bla = (test ++ test2 ++ test3)  
        run prog test
        -- run prog d


-- main = do
    -- Hw2
    -- let n = 2^5
    -- let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n 
    -- -- print $ split [1..64]
    -- -- print(rd 3 s1)
    -- start <- getCurrentTime
    -- let dft1 = map (\x -> x/n) $ absolute $ dft s1 
    -- print(rd 2 dft1)
    -- end <- getCurrentTime
    -- print (diffUTCTime end start)
    -- start2 <- getCurrentTime
    -- let fft1 = map (\x -> x/n) $ absolute $ fft s1 
    -- print(rd 2 fft1)
    -- end2 <- getCurrentTime
    -- print (diffUTCTime end2 start2)
    -- let v1 = Vec [1.0,2.0,3.0]
    -- let v2 = Vec [2,3,4]
    -- let v3 = Vec [-10,0,10]

    -- Hw3
    -- print $ v1 == v2
    -- print $ show v3
    -- print $ v1 + v2
    -- print $ v1 - v2
    -- print $ v1 * v2
    -- print $ v1 / v3
    -- print $ negate v1
    -- print $ signum v3
    -- print $ abs v3
    -- print $ foldr (*) 1 v1
    -- print $ sin $ v1 * (pi / 2)
    -- print $ v1 + (pure' $ sqrt 2)
    -- print $ realV v1
    -- print $ pure' 'a'

    -- let v1 = Vec [1.123,2.213,3.321]
    -- let v2 = Vec [2,3,4]
    -- let v3 = Vec [-10,0,10]
    -- print $ v1 + v2
    -- print $ v1 - v2
    -- print $ v1 * v2
    -- print $ v1 / v2
    -- print $ negate v1
    -- print $ signum v3
    -- print $ abs v3
    -- print $ v1 + 10
    -- print $ v2 + 1.2
    -- print $ v1 + (pure' $ sqrt 2)
    -- print $ realV v1
    -- print $ imagV v1
    -- print $ realV v1 + imagV v2
    -- print $ sin $ v1 * (pi / 2)
    -- print $ sum v1

    -- Hw4
    -- let n = fromIntegral 2^8
    -- let s1 = realV $ Vec $ fmap (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n 
    -- print $ fmap (\z -> conjugate z) s1
    -- print s1
    -- start <- getCurrentTime
    -- let dft1 = fmap (/n) $ absolute $ dft s1
    -- print(rd 2 dft1)
    -- let n = fromIntegral 2^8
    -- let s1 = fmap (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ Vec $ range 0 1 n 
    -- print(rd 3 s1)
    -- print s1
    -- let dft1 = fmap (/n) $ absolute $ dft s1
    -- print(rd 2 dft1)
    -- print(rd 2 $ fmap (/n) $ absolute $ idft $ dft s1)
    -- print(rd 2 $ fmap (/n) $ absolute $ ifft $fft s1)
    
    -- end <- getCurrentTime
    -- print (diffUTCTime end start)
    -- start2 <- getCurrentTime
    -- let fft1 = fmap (/n) $ absolute $ fft s1
    -- print(rd 2 fft1)
    -- end2 <- getCurrentTime
    -- print (diffUTCTime end2 start2)

    -- let v1 = Vec [1,2,3]
    -- print $ v1 + v2
    -- print $ rd 2 v1
    -- let n = fromIntegral 2^8
    -- let s1 = fmap (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ Vec $ range 0 1 n 
    -- print(rd 3 s1)
    -- print(rd 3 $ fmap (\(r:+_) -> r) $ low_pass' 15 $ realV s1) 
    -- print(rd 3 $ fmap (\(r:+_) -> r) $ low_pass 15 $ realV s1)
    -- let a = [2,4,1,11,9]
    -- let b = [3,1,5,0,2]
    -- print $ maxList a  
    -- print $ maxList' (<) a
    -- print $ rd 1 $ makeNoise 12345 10 0.0 1.0
    -- let n' = 2^8
    -- let noise = makeNoise 0 n' (-1.0) 1.0
    -- let n = fromIntegral n'
    -- let s = fmap (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ Vec $ range 0 1 n 
    -- let s1 = s + noise
    -- -- noisy signal 
    -- print(rd 2 s1)
    -- -- noisy frequency
    -- print (rd 2 $ fmap (/n) $ absolute $ dft $ realV s1)
    -- -- filtered signal (dft)
    -- print (rd 3 $ fmap (\(r:+_) -> r) $ low_pass' 15 $ realV s1)
    -- -- filtered signal (fft)
    -- print (rd 2 $ fmap (\(r:+_) -> r) $ low_pass 15 $ realV s1)