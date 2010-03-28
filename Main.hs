{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Evaluate
import Syntax
import Renaming
import Utilities

import Control.Monad
import Data.Data
import Data.Maybe
import Data.Generics.Uniplate.Data
import System.IO


finish :: Step -> ([Step], Term)
finish s = first (s :) $ case s of
    Eval rn k e -> finish (eval' rn k e)
    RebuildEvaluationContext k a -> finish (rebuildEvaluationContext' k a)
    Done r -> ([], case r of Answer heap rn v -> letRec [(x', renameValue (joinRenaming rn_v (inScopeSet rn)) v) | (x', rn_v, v) <- flattenHeap heap] (Value (renameValue rn v))
                             Force _k_i _iss _x' -> undefined) -- TODO: handle open terms


filterChain :: [Step] -> [Step]
filterChain [] = []
filterChain (s:ss) = case s of
    Eval _ _ _                   -> s : filterChain (dropWhile isEval ss)
    RebuildEvaluationContext _ _ -> filterChain (dropWhile isRebuildEvaluationContext ss)
    Done _                       -> s : filterChain ss
  where
    isEval (Eval {}) = True
    isEval _ = False
    
    isRebuildEvaluationContext (RebuildEvaluationContext {}) = True
    isRebuildEvaluationContext _ = False


tidyStep :: Step -> Step
tidyStep = rewriteBi f
  where
    {-
    transformTopDownBi :: (Biplate from to, Uniplate to) => (to -> to) -> from -> from
    transformTopDownBi f x = descendBi (descend f . f) x
    -}

    takeHeapGroups k = (map (fromJust . fromNonRecHeapGroupBindFrame) k_hgs, k')
      where (k_hgs, k') = span (isJust . fromNonRecHeapGroupBindFrame) k
            fromNonRecHeapGroupBindFrame (HeapBindFrame (HeapGroup NonRec xrnvs1)) = Just xrnvs1
            fromNonRecHeapGroupBindFrame _ = Nothing
    
    takeLetBinds = span suitable
      where suitable (ValueConsumerFrame (LetBind _ _ _)) = True
            suitable _ = False
    
    f :: EvaluationContext -> Maybe EvaluationContext
    f k0 = go [] False k0
      where
        go hgs floated_lets k0
          | null hgs' = case hgs of _:(_:_) -> Just (HeapBindFrame (HeapGroup NonRec (concat $ reverse hgs)) : k0); _ -> Nothing
          | otherwise = fmap (let_ks ++) $ go (hgs ++ hgs') (floated_lets || not (null let_ks)) k2
          where
            (hgs', k1) = takeHeapGroups k0
            (let_ks, k2) = takeLetBinds k1


main :: IO ()
main = do
    hPutStrLn stderr "Evaluating:"
    hPrint stderr e
    
    hPutStrLn stderr ""
    
    let t = finish $ eval [] emptyRenaming e -- NB: don't use rn to minimise renaming
    
    hPutStrLn stderr "Evaluation:"
    
    mapM_ putStrLn [
        "<html>",
        "<style type=\"text/css\">",
        ".focused { color: red }",
        ".cofocused { color: blue }",
        "</style>",
        "<head>",
        "</head>",
        "<body>"
      ]
    
    forM_ (filterChain $ fst t) $ \(tidyStep -> step) -> do
        putStrLn "<pre>"
        print $ pPrint step
        putStrLn "</pre>"
        putStrLn ""
        -- putStrLn "<p>"
        -- putStrLn (show step)
        -- putStrLn "</p>"
        -- putStrLn ""
        putStrLn "<br/>"
    
    mapM_ putStrLn [
        "</body>",
        "</html>"
      ]
    
    hPutStrLn stderr "Result:"
    hPrint stderr $ pPrint $ snd t
  where
    nilDataCon = "[]"
    consDataCon = "(:)"
    list = foldr (\l ls -> Var cons_wrap `App` l `App` ls) (Value (Data nilDataCon []))
    
    (_rn, [cons_wrap, map, f, xs, y, ys, z, zs]) = freshBinders emptyRenaming ["op::", "map", "f", "xs", "y", "ys", "z", "zs"]
    e = LetRec [(cons_wrap, Lambda y $ Value $ Lambda ys $ Value (Data consDataCon [y, ys])),
                (map, Lambda f $ Value $ Lambda xs $
                          Case (Var xs) [(DataAlt nilDataCon  [],
                                            Value (Data nilDataCon [])),
                                         (DataAlt consDataCon [z, zs],
                                            Var cons_wrap `App` (Var f `App` Var z) `App` (Var map `App` Var f `App` Var zs))])] $
        (Var map `App` (Value $ Lambda y $ Value (PrimOp Add [Literal 1]) `App` Var y) `App` list [Value (Literal 1), Value (Literal 2), Value (Literal 3)])
