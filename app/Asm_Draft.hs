{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}

module Asm_Draft
( toAsm
, livenessAnalysis
, ppAsm
, exp1
, exp2
) where

import Data.Char (toUpper)
import qualified Data.Map as M
import Control.Lens.Indexed as Ind

-----------------
-- Definitions --
-----------------

exp1 :: Expr
exp1 = EBinOp Mul
        (EBinOp Add
          (EVar "a")
          (EVar "b")
        )
        (EBinOp Add
          (EVar "a")
          (ELit 2)
        )

exp2 :: Expr
exp2 = EBinOp Div
        (EUnaryOp Neg (ELit 2))
        (EBinOp Add
          (EBinOp Add
            (EBinOp Sub
              (EVar "a")
              (EVar "b")
            )
            (EVar "c")
          )
          (EBinOp Sub
            (EBinOp Mul (ELit 2) (EVar "b"))
            (EVar "c")
          )
        )

-- Expression --

data Expr
  = EBinOp BinOp Expr Expr
  | EUnaryOp UnaryOp Expr
  | ELit Int
  | EVar String
  deriving (Show, Eq, Ord)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq, Ord)

data UnaryOp
  = Neg
  deriving (Show, Eq, Ord)

-- Assembly --

type Asm = [Instruction]

data Instruction
  = AsmLoad Reg String 
  | AsmBinOp BinOp Reg Imm Imm
  | AsmUnaryOp UnaryOp Reg Imm
  | AsmRet Imm
  deriving (Show, Eq, Ord)

data Imm
  = ImmLit Int
  | ImmReg Reg
  deriving (Show, Eq, Ord)

newtype Reg = Reg Int
  deriving (Show, Eq, Ord)

-- Pretty Printing --

ppAsm :: Asm -> IO ()
ppAsm = mapM_ (putStrLn . ppInstruction)

ppInstruction :: Instruction -> String
ppInstruction = \case
  AsmLoad target addr ->
    ppReg target <> " <- LOAD " <> addr

  AsmBinOp op target i1 i2 ->
    unwords [ppReg target, "<-", map toUpper (show op), ppImm i1, ppImm i2]

  AsmUnaryOp op target i ->
    unwords [ppReg target, "<-", map toUpper (show op), ppImm i]

  AsmRet i ->
    "ret " <> ppImm i

ppImm :: Imm -> String
ppImm = \case
  ImmLit i -> show i
  ImmReg r -> ppReg r

ppReg :: Reg -> String
ppReg (Reg i) = "%r" <> show i

-------------------------
-- Convert Expr to Asm --
-------------------------

-- 1. Implement: toAsm :: Expr -> Asm


toAsm :: Expr -> Asm
toAsm expr = loadVars ++ useVars ++ [AsmRet imm]
  where (useVars, imm, AsmState e _) = toAsm' expr (AsmState M.empty 0)
        loadFold k a asm = AsmLoad (Reg a) k : asm
        loadVars = M.foldrWithKey loadFold [] e

type Env = M.Map String Int
data AsmState = AsmState Env Int

lookupOrUpdate :: String -> AsmState -> (Int, AsmState)
lookupOrUpdate key s@(AsmState e reg) = case val of
    Nothing -> (reg, AsmState (M.insert key reg e) (succ reg))
    Just x -> (x, s)
  where val = M.lookup key e


-- TODO: 1. avoid ++ if possible
--       2. see if monads help here
toAsm' :: Expr -> AsmState -> (Asm, Imm, AsmState)
toAsm' (ELit x) s = ([],ImmLit x, s)
toAsm' (EVar name) s =
  let (reg, s') = lookupOrUpdate name s
  in ([], ImmReg (Reg reg), s')
toAsm' (EUnaryOp op expr) s =
  let (asm, imm, AsmState e reg) = toAsm' expr s
      asm' = asm ++ [AsmUnaryOp op (Reg reg) imm]
  in
    (asm', ImmReg (Reg reg), AsmState e (succ reg))
toAsm' (EBinOp op ex1 ex2) s =
  let (asm1, imm1, s1) = toAsm' ex1 s
      (asm2, imm2, s2) = toAsm' ex2 s1
      AsmState e reg = s2
      asm = asm1 ++ asm2 ++ [AsmBinOp op (Reg reg) imm1 imm2]
  in
    (asm, ImmReg (Reg reg), AsmState e (succ reg))


-----------------------
-- Liveness Analysis --
-----------------------

-- 2. Implement: livenessAnalysis :: Asm -> M.Map Reg Line
--    Which will describe what is the last occurence of each register

type Line = Int

livenessAnalysis :: Asm -> M.Map Reg Line
livenessAnalysis = Ind.ifoldr livenessFold M.empty

livenessFold :: Int -> Instruction -> M.Map Reg Line -> M.Map Reg Line
livenessFold i inst = flip M.union (M.fromList $ zip (extractRegs inst) (repeat i))

extractRegs :: Instruction -> [Reg]
extractRegs (AsmLoad reg _) = [reg]
extractRegs (AsmBinOp _ reg imm1 imm2) = reg : (immToReg imm1 ++ immToReg imm2)
extractRegs (AsmUnaryOp _ reg imm) = reg : immToReg imm
extractRegs (AsmRet imm) = immToReg imm

immToReg :: Imm -> [Reg]
immToReg (ImmLit _) = []
immToReg (ImmReg reg) = [reg]
