module ParseWhile where 

import Control.Applicative

import Parser
import Syntax

true :: Parser Bool
true = token "true" *> pure True

false :: Parser Bool
false = token "false" *> pure False

bool :: Parser Bool
bool = true <|> false

bLit :: Parser Lit
bLit = LBool <$> bool

iLit :: Parser Lit
iLit = LInt <$> natural <* ws

lit :: Parser Lit
lit = bLit <|> iLit

name :: Parser Name
name = some lowerAlpha <* ws

-- TODO: use lowerAlpha (not token)
var :: Parser Var
var = Var <$> name

-- eLit :: Parser Expr
-- eLit = ELit <$> lit

-- eVar :: Parser Expr
-- eVar = EVar <$> var

-- TODO: ELit, EVar

expr' :: Parser Expr
expr' =  ELit <$> lit 
     <|> EVar <$> var
     <|> Not  <$> (token "!" *> expr')
     <|> token "(" *> expr <* token ")"

expr :: Parser Expr
expr =  LEq   <$> (expr' <* token "<=") <*> expr
    <|> Eq    <$> (expr' <* token "==") <*> expr
    -- arithmetics
    <|> Plus  <$> (expr' <* token "+")   <*> expr
    <|> Minus <$> (expr' <* token "-")   <*> expr
    <|> Mul   <$> (expr' <* token "*")   <*> expr
    --  atoms
    <|> expr'

statement' :: Parser Statement --skip
statement' = (\_ -> Skip) <$> (token "Skip")
          <|> Assign      <$> (var <* token ":=") <*> expr
          <|> If          <$> (token "If"    *> expr) <*> statement <*> (token "Else" *> statement)
          <|> While       <$> (token "While" *> expr) <*> statement

statement :: Parser Statement
statement = Seq <$> statement' <*> statement
         <|> statement'


program :: Parser Statement
program = statement <* eof