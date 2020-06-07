module Main where

import Asm

-- (a + b) * (a + 2)
exp1 = (var "a" `add` var "b") `mul` (var "a" `add` lit 2)

-- -2 / (a - b + c + 2*b - c)
exp2 = neg (lit 2) `ddiv` (var "a" `sub` var "b" `add` var "c" `add` (lit 2 `mul` var "b") `sub` var "c")

main :: IO ()
main = do
  handleExpr exp1
  handleExpr exp2

handleExpr :: Expr -> IO ()
handleExpr expr = do
  putStrLn "================="
  let asm = toAsm expr
  ppAsm asm
  print $ livenessAnalysis asm
  