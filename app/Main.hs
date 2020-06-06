module Main where

import Asm_Draft


main :: IO ()
main = do
  putStrLn "============"
  let asm1 = toAsm exp1
      asm2 = toAsm exp2
  ppAsm asm1
  print $ livenessAnalysis asm1
  putStrLn "============"
  ppAsm asm2
  print $ livenessAnalysis asm2

