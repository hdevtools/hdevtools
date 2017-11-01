{-# Language ScopedTypeVariables, CPP #-}

module FindSymbol
    ( findSymbol
    ) where

#if __GLASGOW_HASKELL__ >= 802
import GhcMonad (liftIO)
#elif __GLASGOW_HASKELL__ >= 710
import GHC.PackageDb (exposedName)
import GhcMonad (liftIO)
#else
import Control.Applicative ((<$>))
import qualified UniqFM
#endif

import Control.Exception
import Control.Monad (filterM)
import Data.List (find, nub)
import Data.Maybe (catMaybes, isJust)
import Exception (ghandle)

import qualified GHC
import qualified Packages as PKG
import qualified Name

type SymbolName = String
type ModuleName = String

findSymbol :: SymbolName -> GHC.Ghc [ModuleName]
findSymbol symbol = do
   fileMods <- findSymbolInFile symbol

   pkgsMods <- findSymbolInPackages symbol
   return . nub . map (GHC.moduleNameString . GHC.moduleName) $ fileMods ++ pkgsMods


findSymbolInFile :: SymbolName -> GHC.Ghc [GHC.Module]
findSymbolInFile symbol =
   filterM (containsSymbol symbol) =<< map GHC.ms_mod <$> GHC.getModuleGraph


findSymbolInPackages :: SymbolName -> GHC.Ghc [GHC.Module]
findSymbolInPackages symbol =
   filterM (containsSymbol symbol) =<< allExposedModules
   where
   allExposedModules :: GHC.Ghc [GHC.Module]
   allExposedModules = do
      modNames <- exposedModuleNames
      catMaybes <$> mapM findModule modNames
      where
      exposedModuleNames :: GHC.Ghc [GHC.ModuleName]
#if __GLASGOW_HASKELL__ >= 802
      exposedModuleNames = do
        dynFlags <- GHC.getSessionDynFlags
        pkgConfigs <- liftIO $ fmap concat
          . (fmap . fmap) snd . PKG.readPackageConfigs $ dynFlags
        return $ map fst (concatMap exposedModules pkgConfigs)
#elif __GLASGOW_HASKELL__ >= 800
      exposedModuleNames = do
        dynFlags <- GHC.getSessionDynFlags
        pkgConfigs <- liftIO $ fmap concat
          . (fmap . fmap) snd . PKG.readPackageConfigs $ dynFlags
        return $ map exposedName (concatMap exposedModules pkgConfigs)
#elif __GLASGOW_HASKELL__ >= 710
      exposedModuleNames = do
        dynFlags <- GHC.getSessionDynFlags
        pkgConfigs <- liftIO $ PKG.readPackageConfigs dynFlags
        return $ map exposedName (concatMap exposedModules pkgConfigs)
#else
      exposedModuleNames =
         concatMap exposedModules
                   . UniqFM.eltsUFM
		   . PKG.pkgIdMap
		   . GHC.pkgState
		   <$> GHC.getSessionDynFlags
#endif

      exposedModules pkg = if PKG.exposed pkg then PKG.exposedModules pkg else []

      findModule :: GHC.ModuleName -> GHC.Ghc (Maybe GHC.Module)
      findModule moduleName =
         ghandle (\(_ :: SomeException) -> return Nothing)
                 (Just <$> GHC.findModule moduleName Nothing)


containsSymbol :: SymbolName -> GHC.Module -> GHC.Ghc Bool
containsSymbol symbol module_ =
   isJust . find (== symbol) <$> allExportedSymbols
   where
   allExportedSymbols =
      ghandle (\(_ :: SomeException) -> return [])
              (do info <- GHC.getModuleInfo module_
                  return $ maybe [] (map Name.getOccString . GHC.modInfoExports) info)
