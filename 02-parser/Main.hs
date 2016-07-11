{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC
import GHC.Paths (libdir)

import DynFlags
import Outputable
import HscTypes
import CorePrep
import CoreToStg

import Control.Monad.Trans

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

banner :: MonadIO m => String -> m ()
banner msg = liftIO $ putStrLn (
  (replicate (fromIntegral n) '=')
  ++
  msg
  ++ 
  (replicate (fromIntegral n) '=')
  )
  where
    n = (76 - length msg) `div` 2

main :: IO ()
main = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted }

  target <- guessTarget "Example.hs" Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName "Example"

  pmod <- parseModule modSum      -- ModuleSummary
  tmod <- typecheckModule pmod    -- TypecheckedSource
  dmod <- desugarModule tmod      -- DesugaredModule
  let core = coreModule dmod      -- CoreModule
  stg <- liftIO $ coreToStg dflags (mg_module core) (mg_binds core)

  liftIO $ banner "Parsed Source"
  liftIO $ putStrLn $ showGhc ( parsedSource pmod )

  liftIO $ banner "Renamed Module"
  liftIO $ putStrLn $ showGhc ( tm_renamed_source tmod )

  liftIO $ banner "Typechecked Module"
  liftIO $ putStrLn $ showGhc ( tm_typechecked_source tmod )

  liftIO $ banner "Typed Toplevel Definitions"
  liftIO $ putStrLn $ showGhc ( modInfoTyThings (moduleInfo tmod) )

  liftIO $ banner "Typed Toplevel Exports"
  liftIO $ putStrLn $ showGhc ( modInfoExports (moduleInfo tmod) )

  liftIO $ banner "Core Module"
  liftIO $ putStrLn $ showGhc ( mg_binds core )

  liftIO $ banner "STG"
  liftIO $ putStrLn $ showGhc stg
