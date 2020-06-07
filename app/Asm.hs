{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}

module Asm
( toAsm
, livenessAnalysis
, ppAsm
, Expr
, add
, sub
, mul
, ddiv
, neg
, var
, lit
) where

import Data.Char (toUpper)
import qualified Data.Map as M
import Control.Lens.Indexed as Ind
import Control.Monad.State

-----------------
-- Definitions --
-----------------

var :: String -> Expr
var = EVar

lit :: Int -> Expr
lit = ELit

neg :: Expr -> Expr
neg = EUnaryOp Neg

add :: Expr -> Expr -> Expr
add = EBinOp Add

sub :: Expr -> Expr -> Expr
sub = EBinOp Sub

mul :: Expr -> Expr -> Expr
mul = EBinOp Mul

ddiv :: Expr -> Expr -> Expr
ddiv = EBinOp Div


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

-- | transform a given expression into a list of assembly instructions
toAsm :: Expr -> Asm
toAsm expr = loadVars ++ useVars ++ [AsmRet imm]
  where ((useVars, imm), AsmState e _) = runState (toAsmS expr) (AsmState M.empty (Reg 0))
        loadFold name reg asm = AsmLoad reg name : asm
        loadVars = M.foldrWithKey loadFold [] e

-- | Env holds the environment of all registers bound to variable addresses
type Env = M.Map String Reg
-- | The state of the transformation holds both the environment and the
-- amount of registers allocated so far
data AsmState = AsmState Env Reg

-- | Either return the register that is bound to the variable
-- or will bind a new register to that variable
lookupVar :: String -> State AsmState Reg
lookupVar name = do
  (AsmState e _) <- get
  case M.lookup name e of
    Just reg -> return reg
    Nothing -> allocateVar name

-- | Bind the next available register to the named variable
allocateVar :: String -> State AsmState Reg
allocateVar name = do
  (AsmState e reg) <- get
  put $ AsmState (M.insert name reg e) (nextReg reg)
  return reg

-- | allocate a register
allocateReg :: State AsmState Reg
allocateReg = do
  (AsmState e reg) <- get
  put $ AsmState e (nextReg reg)
  return reg

-- | This is needed because Reg is not deriving from Enum
nextReg :: Reg -> Reg
nextReg (Reg x) = Reg (succ x)

-- | create a state monad that will transform the expression into
-- a tuple of: assembly (without loading) and the final immediate to return
-- the resulting AsmState hold the environment with all bound registers
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
livenessFold line inst = flip M.union (M.fromList $ zip (extractRegs inst) (repeat line))

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
