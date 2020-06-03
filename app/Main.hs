module Main where

import Asm_Draft


main :: IO ()
main = do
  putStrLn "============"
  ppAsm . toAsm $ exp1
  putStrLn "============"
  ppAsm . toAsm $ exp2

