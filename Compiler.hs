module Compiler where

import Data.Int
import Data.List
import Data.Maybe

import LL.Language as LL
import X86 as X86

import Debug.Trace


compile :: LL.Prog -> X86.Prog
compile p =
    compileGlobals (types p) (globals p) ++
    compileFunctions (types p) (functions p)

sizeOf :: Types -> Type -> Int64
sizeOf _ Void                    = 0
sizeOf _ I1                      = 8
sizeOf _ I8                      = 8
sizeOf _ I32                     = 8
sizeOf _ I64                     = 8
sizeOf _ (Ptr _)                 = 8
sizeOf named (Struct ts)         = sum $ map (sizeOf named) ts
sizeOf _ (Array n I8)            = n
sizeOf named (Array n (Named s)) = sizeOf named (Array n (fromJust (lookup s named)))
sizeOf named (Array n t)         = n * sizeOf named t
sizeOf _ (Fun _ _)               = 0
sizeOf named (Named s)           = sizeOf named (fromJust (lookup s named))

compileGlobals :: Types -> Globals -> X86.Prog
compileGlobals named = map (compileGlobal named)

compileGlobal named (label, _, init) = (label, True, Data (compileInit init))
    where compileInit (INull) = [Word (Literal 0)]
          compileInit (IGid label) = [Word (Label label)]
          compileInit (IInt i) = [Word (Literal i)]
          compileInit (IString s) = [String s]
          compileInit (IArray inits) = concatMap compileInit (map snd inits)
          compileInit (IStruct inits) = concatMap compileInit (map snd inits)

compileFunctions :: Types -> Functions -> X86.Prog
compileFunctions named = concatMap (compileFunction named)


-- You may find the following function helpful in implementing operations on temporary storage
-- locations

type TemporaryMap = [(String, X86.SourceOperand)]
temporaries :: Cfg -> [String]
temporaries  (first, rest) = concatMap (temporaries') blocks
        where blocks = ("^", first) : rest

temporaries' :: (String, Block) -> [String]
temporaries' (n, (instrs, term)) = concatMap getDefs instrs

getDefs :: LL.Instruction -> [String]
getDefs (Bin def _ _ _ _)         = [def]
getDefs (Alloca def _ )           = [def]
getDefs (Load def _ _ )           = [def]
getDefs (Store _ _ _ )           = []
getDefs (Icmp def _ _ _ _   )     = [def]
getDefs (Call def _ _ [(_, _)])   = [def]
getDefs (Bitcast def _ _ _ )      = [def]
getDefs (Gep def _ _ [_])         = [def]




compileFunction :: Types -> (String, Function) -> X86.Prog
compileFunction = error "Unimplemented"

compileBlock :: Types -> TemporaryMap -> Block -> [X86.SourceInstr]
compileBlock = error "Unimplemented"

compileOperand :: TemporaryMap -> LL.Operand -> X86.SourceOperand
compileOperand tempMap (Const n) = Imm (Literal n)
compileOperand tempMap (Gid n) = IndImm (Label n)
compileOperand tempMap (Uid n) = fromJust (lookup n tempMap)


compileInstr :: Types -> TemporaryMap -> LL.Instruction -> [X86.SourceInstr]
compileInstr ts tempMap (Bin dst oprt _ opd1 opd2) = ... -- %uid = binop t op, op
compileInstr ts tempMap (Alloca al ty) = [subq -- use sizeOf on top
                                         , movq]                       -- %uid = alloca t
compileInstr ts tempMap   Load String Type Operand                  -- %uid = load t, t* op
compileInstr ts tempMap   Store Type Operand Operand                -- store t op1, t* op2
compileInstr ts tempMap   Icmp String Condition Type Operand Operand -- %uid = icmp rel t op1 op2
compileInstr ts tempMap   Call String Type String [(Type, Operand)] -- %uid = call ret_ty name(t1 op1, t2 op2, ...)
compileInstr ts tempMap   Bitcast String Type Operand Type          -- %uid = bitcast t1 op to t2
compileInstr ts tempMap   Gep String Type Operand [Operand]


compileTerm :: TemporaryMap -> Terminator -> [X86.SourceInstr]
compileTerm tempMap (Ret _ Nothing) = [ movq ~%RBP ~%RSP
                                      , popq ~%RBP
                                      , retq]
compileTerm tempMap (Ret _ (Just op)) = [ movq ~~(compileOperand tempMap op) ~%RAX -- for ll operands
                                        , movq ~%RBP ~%RSP
                                        , popq ~%RBP
                                        , retq]
compileTerm tempMap (Bra n) = [jmp ~$$ n]
compileTerm tempMap (CBr op l1 l2) = [ cmpq ~$1 ~~(compileOperand tempMap op)
                                     , j Eq ~$$ l1
                                     , jmp ~$$ l2
                                     ]
