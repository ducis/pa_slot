module Syntax.Slot where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

slot = s

s = QuasiQuoter { 
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined}
