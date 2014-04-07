{-# LANGUAGE LambdaCase, ViewPatterns,ScopedTypeVariables, NoMonomorphismRestriction #-}
module Syntax.Slot where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.Meta as M
import Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.Annotated.Simplify
import Language.Haskell.Exts.SrcLoc
import Data.Generics
import Data.Data
import Control.Applicative
import Text.Read
import Data.Either
import Text.Printf
import qualified Data.Vector as V
import Data.List
import qualified Data.Map as M
import Debug.Trace(trace)

--tr x = trace (show x) x
tr x = x

slot = s

s = QuasiQuoter onExp e e e where
	e _ = fail "Not here"
	onExp s = case parseExp s of 
		ParseOk a -> transform a
		ParseFailed _ s -> fail s
	transform exp = do
		let 
			f = \case
				Var _ (UnQual _ (A.Ident (l::SrcSpanInfo) x)) -> case x of
					"ı" -> [Left (0,l)]
					"_ı" -> [Left (1,l)]
					'_':(readMaybe -> Just (n::Int)) -> [Right (n,l)]
				_ -> []
			((groupBy (\(a,_) (b,_)->a+b==1)->gs), ns) = partitionEithers $ everything (++) (mkQ [] f) exp
			ms = [0..(maximum $ tr (length gs-1): map fst ns)]
		names <- (`mapM` ms) $ (show <$>). newName.printf "slot_%02d"
		let namesV = V.fromList names
		let namesI = M.fromList $ sort $ concat $ zipWith (\n g->[(l,n)|(_,l)<-g]) names gs
		return $ M.toExp $ sExp $ (`everywhere` exp) $ mkT $ \case
			Var l0 (UnQual l1 (A.Ident (l::SrcSpanInfo) x)) -> Var l0 $ UnQual l1 $ A.Ident l $ case x of
				"ı" -> fromI
				"_ı" -> fromI
				'_':(readMaybe -> Just (n::Int)) -> namesV V.! n
				x -> x
				where 
				fromI = namesI M.! l
			x -> x
			--Var l (UnQual _ (show->x)) -> case x of

branch f r = case r of
	Right e -> f e
	Left err -> fail err
