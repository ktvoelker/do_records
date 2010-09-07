
{-# LANGUAGE TemplateHaskell #-}
module DoRecord ((:-)(..), doRec) where

import Language.Haskell.TH
import Prelude hiding (fst, snd, unzip)
import qualified Prelude as P

class Tuple2 a where
  fst :: a b c -> b
  snd :: a b c -> c

instance Tuple2 (,) where
  fst = P.fst
  snd = P.snd

data (:-) a b = (:-) a b

infixr 0 :-

instance Tuple2 (:-) where
  fst (a :- _) = a
  snd (_ :- b) = b

unzip :: (Tuple2 a) => [a b c] -> ([b], [c])
unzip []       = ([], [])
unzip (x : xs) = case unzip xs of (bs, cs) -> (fst x : bs, snd x : cs)

doRec :: (Tuple2 a) => Name -> [a Name (Q Exp)] -> Q Exp
doRec con binds = do
  let (recNames, recExprs) = unzip binds
  innerNames <- mapM (newName . nameBase) recNames
  let doBinds  = map (\(i, e) -> bindS (varP i) e) $ zip innerNames recExprs
  let recBinds = zip recNames $ map VarE innerNames
  let recRet   = NoBindS $ AppE (VarE $ mkName "return") $ RecConE con recBinds
  doE (doBinds ++ [return recRet])

