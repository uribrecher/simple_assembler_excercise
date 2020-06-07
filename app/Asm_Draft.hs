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
import Control.Monad.State

-----------------
-- Definitions --
-----------------

-- (a + b) * (a + c)
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

-- -2 / (a - b + c + 2*b - c)
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
  where ((useVars, imm), AsmState e _) = runState (toAsmS expr) (AsmState M.empty (Reg 0))
        loadFold name reg asm = AsmLoad reg name : asm
        loadVars = M.foldrWithKey loadFold [] e

type Env = M.Map String Reg
data AsmState = AsmState Env Reg

lookupVar :: String -> State AsmState Reg
lookupVar name = do
  (AsmState e _) <- get
  case M.lookup name e of
    Just reg -> return reg
    Nothing -> allocateVar name

allocateVar :: String -> State AsmState Reg
allocateVar name = do
  (AsmState e reg) <- get
  put $ AsmState (M.insert name reg e) (nextReg reg)
  return reg

allocateReg :: State AsmState Reg
allocateReg = do
  (AsmState e reg) <- get
  put $ AsmState e (nextReg reg)
  return reg

nextReg :: Reg -> Reg
nextReg (Reg x) = Reg (succ x)

toAsmS :: Expr -> State AsmState (Asm, Imm)
toAsmS = \case
  ELit x -> return ([], ImmLit x)
  EVar name -> do
    reg <- lookupVar name
    return ([], ImmReg reg)
  EUnaryOp op expr -> do
    (asm, imm) <- toAsmS expr
    reg <- allocateReg
    return (asm ++ [AsmUnaryOp op reg imm], ImmReg reg)
  EBinOp op ex1 ex2 -> do
    (asm1, imm1) <- toAsmS ex1
    (asm2, imm2) <- toAsmS ex2
    reg <- allocateReg
    return (asm1 ++ asm2 ++ [AsmBinOp op reg imm1 imm2], ImmReg reg)



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
extractRegs = \case
  AsmLoad reg _ -> [reg]
  AsmBinOp _ reg imm1 imm2 -> reg : (immToReg imm1 ++ immToReg imm2)
  AsmUnaryOp _ reg imm -> reg : immToReg imm
  AsmRet imm -> immToReg imm

immToReg :: Imm -> [Reg]
immToReg = \case
  ImmLit _ -> []
  ImmReg reg -> [reg]
