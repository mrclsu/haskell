module Bead04 where

import Data.Maybe
import Control.Monad

-- Bead assignment 04:
--   Define the function `evalExpr :: Expr -> Maybe Bool`.
--   It should evaluate the input boolean expression in the Maybe monad. 
--   It should fail (and return Nothing) if the expression contains Undefined
--   It should succeed (and return (Just ...)) otherwise.

data Expr = ETrue           
          | EFalse
          | And Expr Expr
          | Undefined

evalExpr :: Expr -> Maybe Bool
evalExpr ETrue = Just True
evalExpr EFalse = Just False
evalExpr Undefined = Nothing
evalExpr (And a b) = do
          x <- evalExpr a
          y <- evalExpr b
          return $ x && y

-- Examples:
--   evalExpr ETrue                              == Just True
--   evalExpr EFalse                             == Just True
--   evalExpr Undefined                          == Nothing
--   evalExpr (And ETrue ETrue)                  == Just True
--   evalExpr (And ETrue EFalse)                 == Just False
--   evalExpr (And ETrue Undefined)              == Nothing
--   evalExpr (And (And Undefined ETrue) EFalse) == Nothing
