{-# LANGUAGE PatternGuards, ViewPatterns #-}
module Evaluate where

import Renaming
import Syntax
import Utilities

import Data.List


data HeapGroup = HeapNonRec (Out Var) PureRenaming (In Value)
               | HeapRec [(Out Var, PureRenaming, In Value)]
type Heap = [HeapGroup]

type EvaluationContext = [EvaluationContextFrame]
type InnerEvaluationContext = EvaluationContext

data EvaluationContextFrame = ValueConsumerFrame ValueConsumer
                            | HeapBindFrame HeapGroup

data ValueConsumer = AppliedTo PureRenaming (In Term)
                   | PrimOpArgument PrimOp [Out Value]
                   | Scrutinise PureRenaming [(AltCon, In Term)]
                   | LetBind PureRenaming (In Var) (In Term)

data Result = Answer Heap Renaming (In Value)
            | Force InnerEvaluationContext InScopeSet (Out Var)

data Step = Eval EvaluationContext Renaming (In Term)
          | RebuildEvaluationContext EvaluationContext Result
          | Done Result


resultInScopeSet :: Result -> InScopeSet
resultInScopeSet (Answer _ rn _) = inScopeSet rn
resultInScopeSet (Force _ iss _) = iss


instance Pretty Step where
    pPrintPrec level prec s = case s of
        Eval k rn e -> pPrintPrecEvaluationContext level prec (inScopeSet rn) k (\level prec -> pPrintFocused "focused" level prec e)
        RebuildEvaluationContext k a -> pPrintPrecEvaluationContext level prec (resultInScopeSet a) k (\level prec -> pPrintFocused "cofocused" level prec a)
        Done a -> pPrintPrec level prec a

instance Pretty Result where
    pPrintPrec level prec (Answer heap rn v) = pPrintPrecHeap level prec (inScopeSet rn) heap (\level prec -> pPrintPrec level prec (renameValue rn v))
    pPrintPrec level prec (Force k_i iss x') = pPrintPrecEvaluationContext level prec iss (reverse k_i) (\level prec -> pPrintPrec level prec x')

pPrintFocused :: Pretty a => String -> PrettyLevel -> Rational -> a -> Doc
pPrintFocused style level prec x = zeroWidthText ("<span class=\"" ++ style ++ "\">") <> pPrintPrec level prec x <> zeroWidthText "</span>"

pPrintPrecHeap :: PrettyLevel -> Rational -> InScopeSet -> Heap -> (PrettyLevel -> Rational -> Doc) -> Doc
pPrintPrecHeap level prec iss heap inside = pPrintPrecLetRec level prec [(x', renameValue (joinRenaming rnv iss) v) | (x', rnv, v) <- flattenHeap heap] inside

pPrintPrecEvaluationContext :: PrettyLevel -> Rational -> InScopeSet -> EvaluationContext
                            -> (PrettyLevel -> Rational -> Doc) -> Doc
pPrintPrecEvaluationContext level prec iss k inside = case k of
  []     -> inside level prec
  (kf:k) -> pPrintPrecEvaluationContext level prec iss k (\level prec -> pPrintPrecEvaluationContextFrame level prec iss kf inside)

pPrintPrecEvaluationContextFrame :: PrettyLevel -> Rational -> InScopeSet -> EvaluationContextFrame
                                 -> (PrettyLevel -> Rational -> Doc) -> Doc
pPrintPrecEvaluationContextFrame level prec iss kf inside = case kf of
    ValueConsumerFrame vc -> case vc of
        AppliedTo rn e        -> pPrintPrecApp level prec inside (\level prec -> pPrintPrec level prec (renameTerm (joinRenaming rn iss) e))
        PrimOpArgument pop vs -> pPrintPrecApp level prec (\level prec -> pPrintPrec level prec (Value (PrimOp pop vs))) inside
        Scrutinise rn alts    -> pPrintPrecCase level prec inside (map (renameAlt (joinRenaming rn iss)) alts)
        LetBind rn x e        -> pPrintPrecLet level prec x inside (\level prec -> pPrintPrec level prec (renameTerm (insertRenaming x x $ joinRenaming rn iss) e))
    HeapBindFrame (HeapNonRec x' rn v) -> pPrintPrecLet level prec x' (\level prec -> pPrintPrec level prec (renameValue (joinRenaming rn iss) v)) inside
    HeapBindFrame (HeapRec xrnvs)      -> pPrintPrecLetRec level prec [(x', renameValue (joinRenaming rn iss) v) | (x', rn, v) <- xrnvs] inside


flattenHeap :: Heap -> [(Out Var, PureRenaming, In Value)]
flattenHeap = concatMap flattenHeapGroup

flattenHeapGroup :: HeapGroup -> [(Out Var, PureRenaming, In Value)]
flattenHeapGroup (HeapNonRec x' rn v) = [(x', rn, v)]
flattenHeapGroup (HeapRec xrnvs)      = xrnvs


eval, eval' :: EvaluationContext -> Renaming -> In Term -> Step
eval = Eval

eval' k rn e = case e of
    Var x   -> rebuildEvaluationContext k $ Force [] (inScopeSet rn) (rename rn x)
    Value v -> rebuildEvaluationContext k $ Answer [] rn v
    App e1 e2 -> eval (ValueConsumerFrame (AppliedTo (pureRenaming rn) e2) : k) rn e1
    LetRec (unzip -> (xs, vs)) e -> eval (HeapBindFrame (HeapRec (zip3 xs' (repeat (pureRenaming rn')) vs)) : k) rn' e
      where (rn', xs') = renameBinders rn xs
    Let x e1 e2 -> eval (ValueConsumerFrame (LetBind (pureRenaming rn) x e2) : k) rn e1
    Case e alts -> eval (ValueConsumerFrame (Scrutinise (pureRenaming rn) alts) : k) rn e


rebuildEvaluationContext :: EvaluationContext -> Result -> Step
rebuildEvaluationContext = RebuildEvaluationContext

rebuildEvaluationContext' [] r = Done r
rebuildEvaluationContext' (ValueConsumerFrame vc:k) r = force vc r k
rebuildEvaluationContext' (HeapBindFrame hg:k)      r = binds hg k r

force :: ValueConsumer -> Result -> EvaluationContext -> Step
force vc (Answer heap rn_v v) k = rebuildValueConsumer rn_v v vc $ map HeapBindFrame heap ++ k
force vc (Force k_i iss x') k   = rebuildEvaluationContext k $ Force (ValueConsumerFrame vc : k_i) iss x'

binds :: HeapGroup -> EvaluationContext -> Result -> Step
binds hg k (Answer heap rn_v v) = rebuildEvaluationContext k (Answer (hg : heap) rn_v v)
binds hg k (Force k_i iss x')
  | Just (_, rn_v, v) <- find (\(y', _, _) -> y' == x') (flattenHeapGroup hg) = rebuildEvaluationContext (reverse k_i ++ HeapBindFrame hg : k) (Answer [] (joinRenaming rn_v iss) v)
  | otherwise                                                                 = rebuildEvaluationContext k (Force (HeapBindFrame hg : k_i) iss x')


rebuildValueConsumer :: Renaming -> In Value -> In ValueConsumer -> EvaluationContext -> Step
rebuildValueConsumer rnv v vc k = case vc of
    AppliedTo rnvc e2
      | PrimOp pop vs <- v -> eval (ValueConsumerFrame (PrimOpArgument pop vs)            : k) (joinRenaming rnvc (inScopeSet rnv)) e2
      | Lambda x e1b  <- v -> eval (ValueConsumerFrame (LetBind (pureRenaming rnv) x e1b) : k) (joinRenaming rnvc (inScopeSet rnv)) e2
    PrimOpArgument pop vs -> rebuildEvaluationContext k $ Answer [] (mkRenaming (inScopeSet rnv)) (primop pop vs (renameValue rnv v))
    Scrutinise rnvc alts
      | Literal l  <- v -> head [eval k (joinRenaming rnvc (inScopeSet rnv)) e | (LiteralAlt l', e) <- alts, l' == l]
      | Data dc xs <- v -> head [eval k (insertRenamings (xs' `zip` map (rename rnv) xs) (joinRenaming rnvc (inScopeSet rnv))) e | (DataAlt dc' xs', e) <- alts, dc' == dc]
    LetBind rnvc x e -> eval (HeapBindFrame (HeapNonRec x' (pureRenaming rnv) v) : k) rnv' e
      where (rnv', x') = renameBinder (joinRenaming rnvc (inScopeSet rnv)) x


primop :: PrimOp -> [Out Value] -> Out Value -> Out Value
primop Add      [Literal i1] (Literal i2) = Literal (i1 + i2)
primop Subtract [Literal i1] (Literal i2) = Literal (i1 - i2)
primop pop      vs           v            = PrimOp pop (vs ++ [v])
