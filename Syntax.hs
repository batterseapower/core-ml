{-# LANGUAGE PatternGuards, ViewPatterns, DeriveDataTypeable #-}
module Syntax where

import Renaming
import Utilities

import Data.Data


type Var = Name

type DataCon = String

data PrimOp = Add | Subtract
            deriving (Eq, Show, Typeable, Data)

data AltCon = DataAlt DataCon [Var] | LiteralAlt Literal
            deriving (Show, Typeable, Data)

type Literal = Int

data Term = Var Var | Value Value | App Term Term | LetRec [(Var, Value)] Term | Let Var Term Term | Case Term [(AltCon, Term)]
          deriving (Show, Typeable, Data)

data Value = Lambda Var Term | PrimOp PrimOp [Value] | Data DataCon [Var] | Literal Literal
           deriving (Show, Typeable, Data)

pPrintPrecDataCon :: PrettyLevel -> Rational -> DataCon -> [Var] -> Doc
pPrintPrecDataCon level prec dc xs
  | level == haskellLevel = prettyParen (notNull xs && prec >= appPrec) $ text dc <+> hsep xs_docs
  | otherwise = prettyParen (notNull xs && prec >= appPrec) $ case (dc, xs_docs) of
      ("Just",    [doc1])       -> text "SOME" <+> doc1
      ("Nothing", [])           -> text "NONE"
      ("(:)",     [doc1, doc2]) -> doc1 <+> text "::" <+> doc2
      ("[]",      [])           -> text "[]"
  where xs_docs = map (pPrintPrec level appPrec) xs

instance Pretty PrimOp where
    pPrint Add      = text "op+"
    pPrint Subtract = text "op-"

instance Pretty AltCon where
    pPrintPrec level prec altcon = case altcon of
        DataAlt dc xs -> pPrintPrecDataCon level prec dc xs
        LiteralAlt l  -> int l

instance Pretty Term where
    pPrintPrec level prec e = case e of
        Var x        -> pPrintPrec level prec x
        Value v      -> pPrintPrec level prec v
        App e1 e2    -> pPrintPrecApp level prec (\level prec -> pPrintPrec level prec e1) (\level prec -> pPrintPrec level prec e2)
        LetRec xvs e -> pPrintPrecLetRec level prec xvs (\level prec -> pPrintPrec level prec e)
        Let x e1 e2  -> pPrintPrecLet level prec x (\level prec -> pPrintPrec level prec e1) (\level prec -> pPrintPrec level prec e2)
        Case e alts  -> pPrintPrecCase level prec (\level prec -> pPrintPrec level prec e) alts

pPrintPrecApp :: PrettyLevel -> Rational -> (PrettyLevel -> Rational -> Doc) -> (PrettyLevel -> Rational -> Doc) -> Doc
pPrintPrecApp level prec inside1 inside2 = prettyParen (prec >= appPrec) $ inside1 level opPrec <+> inside2 level appPrec

pPrintPrecLetRec :: PrettyLevel -> Rational -> [(Var, Value)] -> (PrettyLevel -> Rational -> Doc) -> Doc
pPrintPrecLetRec level prec xvs inside
  | null xvs  = inside level prec
  | otherwise = prettyParen (prec > noPrec) $ hang (if level == haskellLevel then text "let" else text "let") 2 (vcat [text "val rec" <+> pPrintPrec level noPrec x <+> text "=" <+> pPrintPrec level noPrec v | (x, v) <- xvs]) $$ text "in" <+> inside level noPrec <+> text "end"

pPrintPrecLet :: PrettyLevel -> Rational -> Var -> (PrettyLevel -> Rational -> Doc) -> (PrettyLevel -> Rational -> Doc) -> Doc
pPrintPrecLet level prec x inside1 inside2 = pPrintPrecLets level prec [(x, inside1)] inside2

pPrintPrecLets :: PrettyLevel -> Rational -> [(Var, PrettyLevel -> Rational -> Doc)] -> (PrettyLevel -> Rational -> Doc) -> Doc
pPrintPrecLets level prec xinsides inside = prettyParen (prec > noPrec) $ hang (text "let val") 2 (vcat [pPrintPrec level noPrec x <+> text "=" <+> inside level noPrec | (x, inside) <- xinsides]) $$ text "in" <+> inside level noPrec <+> text "end"

pPrintPrecCase :: PrettyLevel -> Rational -> (PrettyLevel -> Rational -> Doc) -> [(AltCon, Term)] -> Doc
pPrintPrecCase level prec scrut alts = case alts of
    [] | level == haskellLevel -> text "undefined"
       | otherwise             -> text "_|_"
    (alt:alts) -> prettyParen (prec > noPrec) $ hang (text "case" <+> scrut level noPrec <+> text "of") 2 $ vcat (nest 2 (pPrintPrecAlt level noPrec alt) : map (\alt -> text "|" <+> pPrintPrecAlt level noPrec alt) alts)

pPrintPrecAlt :: PrettyLevel -> Rational -> (AltCon, Term) -> Doc
pPrintPrecAlt level _ (alt_con, alt_e) = hang (pPrintPrec level noPrec alt_con <+> text "->") 2 (pPrintPrec level noPrec alt_e)

instance Pretty Value where
    pPrintPrec level prec v = case v of
        Lambda x e    -> prettyParen (prec > noPrec) $ text "fn" <+> hsep (map (pPrintPrec level appPrec) (x:xs)) <+> text "=>" <+> pPrintPrec level noPrec e'
          where (xs, e') = collectLambdas e
        PrimOp pop vs -> prettyParen (not (null vs) && prec >= appPrec) $ pPrintPrec level opPrec pop <+> hsep (map (pPrintPrec level appPrec) vs)
        Data dc xs    -> pPrintPrecDataCon level prec dc xs
        Literal l     -> int l


isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

termValue_maybe :: Term -> Maybe Value
termValue_maybe (Value v) = Just v
termValue_maybe _         = Nothing

letRec :: [(Var, Value)] -> Term -> Term
letRec []  e = e
letRec xes e = LetRec xes e

lambdas :: [Var] -> Term -> Term
lambdas = flip $ foldr ((Value .) . Lambda)

apps :: Term -> [Term] -> Term
apps = foldl App

collectLambdas :: Term -> ([Var], Term)
collectLambdas (Value (Lambda x e)) = first (x:) $ collectLambdas e
collectLambdas e                    = ([], e)

renameAlt :: Renaming -> In (AltCon, Term) -> Out (AltCon, Term)
renameAlt rn (alt_con, alt_e) = (alt_con', renameTerm rn' alt_e)
  where (rn', alt_con') = renameAltCon rn alt_con

renameAltCon :: Renaming -> In AltCon -> (Renaming, Out AltCon)
renameAltCon rn_alt alt_con = case alt_con of
    LiteralAlt _          -> (rn_alt, alt_con)
    DataAlt alt_dc alt_xs -> second (DataAlt alt_dc) $ renameBinders rn_alt alt_xs

renameTerm :: Renaming -> In Term -> Out Term
renameTerm rn e = case e of
    Var x -> Var (rename rn x)
    Value v -> Value (renameValue rn v)
    App e1 e2 -> App (renameTerm rn e1) (renameTerm rn e2)
    LetRec (unzip -> (xs, vs)) e -> LetRec (zipWith (\x' v -> (x', renameValue rn' v)) xs' vs) (renameTerm rn' e)
      where (rn', xs') = renameBinders rn xs
    Let x e1 e2 -> Let x' (renameTerm rn e1) (renameTerm rn' e2)
      where (rn', x') = renameBinder rn x
    Case e alts -> Case (renameTerm rn e) (map (renameAlt rn) alts)

renameValue :: Renaming -> In Value -> Out Value
renameValue rn v = case v of
    Lambda x e -> Lambda x' (renameTerm rn' e)
      where (rn', x') = renameBinder rn x
    PrimOp pop vs -> PrimOp pop (map (renameValue rn) vs)
    Data dc xs -> Data dc (map (rename rn) xs)
    Literal l -> Literal l
