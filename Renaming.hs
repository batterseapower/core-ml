{-# LANGUAGE PatternGuards #-}
module Renaming (
    Name(..),
    
    In, Out,
    
    InScopeSet,
    
    PureRenaming,
    pureRename, pureRename_maybe,
    
    Renaming,
    emptyRenaming, mkRenaming,
    splitRenaming, joinRenaming,
    pureRenaming, inScopeSet, extendInScopeSet,
    insertRenaming, insertRenamings, renameBinder, renameBinders, rename,
    freshBinder, freshBinders
  ) where

import Utilities

import Control.Arrow ((&&&))

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Data.List
import Data.Ord
import Data.Unique.Id

import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE uniqAwayIdSupply #-}
uniqAwayIdSupply :: IdSupply
uniqAwayIdSupply = unsafePerformIO $ initIdSupply 'u'

uniqAway :: (Name -> Bool) -> Name -> Name
uniqAway ok = go uniqAwayIdSupply
  where go ids n | ok n      = n
                 | otherwise = case splitIdSupply ids of (ids1, ids2) -> go ids1 $ n { name_id = Just (idFromSupply ids2) }


instance (Pretty k, Pretty a) => Pretty (M.Map k a) where
    pPrintPrec level _ m = vcat [ hang (pPrintPrec level 0 k <+> text "|->") 2 (pPrintPrec level 0 v)
                                | (k, v) <- M.toList m]


data Name = Name {
    name_string :: String,
    name_id :: Maybe Id -- Initially, generate names with no Ids and compare on the String - improves pretty printing only!
  }

selectNameKey :: Name -> Either String Id
selectNameKey n = maybe (Left $ name_string n) Right (name_id n)

instance Show Name where
    show = show . pPrint

instance Eq Name where
    (==) = (==) `on` selectNameKey

instance Ord Name where
    compare = comparing selectNameKey

instance Pretty Name where
    pPrint name = text (name_string name) <> maybe empty (\i -> char '_' <> text (show i)) (name_id name)


type In a = a
type Out a = a


type PureRenaming = M.Map (In Name) (Out Name)

pureRename :: PureRenaming -> In Name -> Out Name
pureRename rn n | Just n' <- pureRename_maybe rn n = n'
                | otherwise                        = error $ show (text "Name" <+> pPrint n <+> text "out of scope! Renaming:" $$ pPrint rn)

pureRename_maybe :: PureRenaming -> In Name -> Maybe (Out Name)
pureRename_maybe rn n = M.lookup n rn


type InScopeSet = S.Set (Out Name)


data Renaming = Renaming {
     -- n |-> n' present in map ==> n in scope in input, rename to n'
     -- n not present in map ==> n not in scope in input
    renaming_rn :: PureRenaming,
     -- Set of variables free in output: used for manufacturing fresh names
     -- INVARIANT: renaming_iss >= range(renaming_rn)
    renaming_iss :: InScopeSet
  } deriving (Show)

instance Pretty Renaming where
    pPrintPrec level prec = pPrintPrec level prec . renaming_rn


emptyRenaming :: Renaming
emptyRenaming = mkRenaming S.empty

mkRenaming :: InScopeSet -> Renaming
mkRenaming iss = Renaming (M.fromList [(n, n) | n <- S.toList iss]) iss

splitRenaming :: Renaming -> (PureRenaming, InScopeSet)
splitRenaming = renaming_rn &&& renaming_iss

joinRenaming :: PureRenaming -> InScopeSet -> Renaming
joinRenaming = Renaming

pureRenaming :: Renaming -> PureRenaming
pureRenaming = renaming_rn

inScopeSet :: Renaming -> InScopeSet
inScopeSet = renaming_iss

extendInScopeSet :: Renaming -> [Out Name] -> Renaming
extendInScopeSet rn ns = rn { renaming_iss = renaming_iss rn `S.union` S.fromList ns }

insertRenaming :: In Name -> Out Name -> Renaming -> Renaming
insertRenaming n n' (Renaming rn iss) = Renaming (M.insert n n' rn) (S.insert n' iss)

insertRenamings :: [(In Name, Out Name)] -> Renaming -> Renaming
insertRenamings = flip $ foldr (uncurry insertRenaming)

renameBinder :: Renaming -> In Name -> (Renaming, Out Name)
renameBinder rn n = (insertRenaming n n' rn, n')
  where n' = uniqAway (`S.notMember` renaming_iss rn) n

renameBinders :: Renaming -> [In Name] -> (Renaming, [Out Name])
renameBinders = mapAccumL renameBinder

rename :: Renaming -> In Name -> Out Name
rename rn n = pureRename (pureRenaming rn) n


freshBinder :: Renaming -> String -> (Renaming, Out Name)
freshBinder rn s = (insertRenaming n' n' rn, n')
  where n = Name s Nothing
        n' = uniqAway (`S.notMember` renaming_iss rn) n

freshBinders :: Renaming -> [String] -> (Renaming, [Out Name])
freshBinders = mapAccumL freshBinder
