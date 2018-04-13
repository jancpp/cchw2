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
getDefs (Store _ _ _ )            = []
getDefs (Icmp def _ _ _ _   )     = [def]
getDefs (Call def _ _ [(_, _)])   = [def]
getDefs (Bitcast def _ _ _ )      = [def]
getDefs (Gep def _ _ [_])         = [def]




compileFunction :: Types -> (String, Function) -> X86.Prog
compileFunction tys (n, fun) = []

compileBlock :: Types -> TemporaryMap -> Block -> [X86.SourceInstr]
compileBlock tys tmap (instr, term) = []

compileOperand :: TemporaryMap -> LL.Operand -> X86.SourceOperand
compileOperand tempMap (Const n) = Imm (Literal n)
compileOperand tempMap (Gid n) = IndImm (Label n)
compileOperand tempMap (Uid n) = fromJust (lookup n tempMap)


compileInstr :: Types -> TemporaryMap -> LL.Instruction -> [X86.SourceInstr]
compileInstr ts tempMap (Bin dst oprtr ty op1 op2) = [] -- %uid = binop t op, op -- case oprtr of
                -- data Operator = Add | Sub | Mul | Shl | Lshr | Ashr | And | Or | Xor
                -- where oprtrMap case oprtr of
                --                 Add -> addq ~%R ~%R
                --                 Sub -> subq ~%R ~%R
                --                 Mul -> imulq ~%R ~%R
                --                 Shl -> shrq ~%R ~%R
                --                 Lshr -> shlq ~%R ~%R
                --                 Ashr -> sarq ~%R ~%R
                --                 And -> andq ~%R ~%R
                --                 Or  -> orq ~%R ~%R
                --                 Xor -> xorq ~%R ~%R
compileInstr ts tempMap (Alloca al ty) = [ subq ~$(sizeOf ts ty) ~%RSP
                                         , movq ~%RSP ~~(fromJust (lookup al tempMap)) ]          -- %uid = alloca t
compileInstr ts tempMap (Load id ty op ) =  [ movq ~~(compileOperand tempMap op) ~%RAX
                                            , movq ~#RAX ~%RAX
                                            , movq ~%RAX ~~(fromJust (lookup id tempMap)) ]              -- %uid = load t, t* op
compileInstr ts tempMap (Store ty op1 op2 ) = [ movq ~~(compileOperand tempMap op1) ~%RAX
                                              , movq ~~(compileOperand tempMap op2) ~%RCX
                                              , movq ~%RAX ~#RCX]            -- store t op1, t* op2
compileInstr ts tempMap (Icmp id cc ty op1 op2) = [ movq ~~(compileOperand tempMap op1) ~%RAX
                                                  , cmpq ~~(compileOperand tempMap op2) ~%RAX
                                                  , set cc ~~(fromJust(lookup id tempMap))] -- %uid = icmp rel t op1 op2
compileInstr ts tempMap (Call id ret_ty name argOps) = regInstrs ++ stackIntrs ++ 
                                                       [ callq ~$$name, movq ~%RAX ~~(fromJust(lookup id tempMap))
                                                       , addq ~$(fromIntegral (length stackOps) * 8) ~%RSP]  -- %uid = call ret_ty name(t1 op1, t2 op2, ...)
                    where argRegs = [RSI, RDI, RCX, RDX, R08, R09]
                          regOps = take 6 argOps
                          stackOps = drop 6 argOps
                          regInstrs = map (\(reg, (t, op)) -> movq ~~(compileOperand tempMap op) ~%reg) (zip argRegs regOps)
                          stackIntrs = concatMap (\(t, op) -> [ movq ~~(compileOperand tempMap op) ~%RAX
                                                              , pushq ~%RAX]) stackOps
compileInstr ts tempMap (Bitcast id ty1 op ty2) =   [ movq ~~(compileOperand tempMap op) ~%RAX -- same as load without the middle part
                                                    , movq ~%RAX ~~(fromJust (lookup id tempMap)) ]         -- %uid = bitcast t1 op to t2
-- compileInstr ts tempMap (Gep id ty op1 [op2]) = []   -- %uid = getelementptr t op, i64 op1, i64 op2
                                                    --    .. or i32, if accessing struct...


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
