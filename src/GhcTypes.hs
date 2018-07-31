{-# LANGUAGE CPP #-}
module GhcTypes
    ( getModSummaries
    , TypecheckI
    , needsTemplateHaskellOrQQ
    ) where

import qualified GHC
import qualified HscTypes
#if __GLASGOW_HASKELL__ < 804
import qualified Var
#endif

getModSummaries :: GHC.Ghc [GHC.ModSummary]
#if __GLASGOW_HASKELL__ >= 804
getModSummaries = HscTypes.mgModSummaries <$> GHC.getModuleGraph
#else
getModSummaries = GHC.getModuleGraph
#endif

#if __GLASGOW_HASKELL__ >= 804
type TypecheckI = GHC.GhcTc
#else
type TypecheckI = Var.Var
#endif

needsTemplateHaskellOrQQ :: GHC.ModuleGraph -> Bool
#if __GLASGOW_HASKELL__ >= 804
needsTemplateHaskellOrQQ = GHC.needsTemplateHaskellOrQQ
#else
needsTemplateHaskellOrQQ = GHC.needsTemplateHaskell
#endif

